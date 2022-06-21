#![feature(rustc_private)]
#![feature(iter_intersperse)]

// rustup component add rustc-dev llvm-tools-preview
// version: 1.63.0-nightly (5435ed691 2022-06-07)

extern crate rustc_error_codes;
extern crate rustc_errors;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use std::{path, process, str};

use rustc_errors::registry;
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_middle::mir;
use rustc_middle::ty::{self, Ty, TyCtxt};
use rustc_session::config::{self, CheckCfg};
use rustc_span::source_map;

struct FormalityGen<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}

impl<'tcx> FormalityGen<'tcx> {
    fn generate_decls(&self) -> String {
        let mut res = String::new();
        for id in self.tcx.hir().items() {
            let item = self.tcx.hir().item(id);
            res.push_str(&self.emit_item_decl(item));
            res.push_str("\n\n");
        }
        res
    }

    fn emit_place(&self, place: &mir::Place) -> String {
        let local = format!("_{}", place.local.index());
        place
            .iter_projections()
            .fold(local, |place, (_, proj)| match proj {
                mir::ProjectionElem::Deref => format!("(* {place})"),
                mir::ProjectionElem::Field(field, _) => {
                    format!("(field {place} {})", field.index())
                }
                mir::ProjectionElem::Index(l) => format!("(index {place} {})", l.index()),
                mir::ProjectionElem::Downcast(_, variant) => {
                    format!("(downcast {place} {})", variant.index())
                }
                _ => unimplemented!(),
            })
    }

    fn emit_operand(&self, operand: &mir::Operand) -> String {
        match operand {
            mir::Operand::Copy(place) => format!("(copy {})", self.emit_place(place)),
            mir::Operand::Move(place) => format!("(move {})", self.emit_place(place)),
            mir::Operand::Constant(ct) => format!(
                "(const {})",
                ct.literal
                    .try_to_scalar()
                    .unwrap_or_else(|| unimplemented!())
            ),
        }
    }

    fn emit_rvalue(&self, rvalue: &mir::Rvalue<'tcx>) -> String {
        match rvalue {
            mir::Rvalue::Use(operand) => format!("(use {})", self.emit_operand(operand)),
            mir::Rvalue::Repeat(operand, ct) => {
                format!(
                    "(repeat {} {})",
                    self.emit_operand(operand),
                    ct.kind()
                        .try_to_scalar_int()
                        .unwrap_or_else(|| unimplemented!())
                )
            }
            mir::Rvalue::Ref(re, bk, place) => {
                let mutability = match bk {
                    mir::BorrowKind::Shared => "()",
                    mir::BorrowKind::Mut { .. } => "mut",
                    _ => unimplemented!(),
                };
                format!(
                    "(ref {} {mutability} {})",
                    self.emit_lifetime(*re),
                    self.emit_place(place)
                )
            }
            mir::Rvalue::AddressOf(mu, place) => {
                let mutability = match mu {
                    mir::Mutability::Not => "()",
                    mir::Mutability::Mut => "mut",
                };
                format!("(addr-of {mutability} {})", self.emit_place(place))
            }
            mir::Rvalue::Len(place) => format!("(len {})", self.emit_place(place)),
            mir::Rvalue::BinaryOp(bin_op, operands) => {
                let op = match bin_op {
                    mir::BinOp::Add => "+",
                    mir::BinOp::Sub => "-",
                    mir::BinOp::Mul => "*",
                    mir::BinOp::Div => "/",
                    _ => unimplemented!(),
                };
                format!(
                    "({op} {} {})",
                    self.emit_operand(&operands.0),
                    self.emit_operand(&operands.1)
                )
            }
            _ => format!("(unknown-rvalue {rvalue:?})"),
        }
    }

    fn emit_statement(&self, stmt: &mir::Statement<'tcx>) -> String {
        match &stmt.kind {
            mir::StatementKind::Assign(ass) => format!(
                "({} = {})",
                self.emit_place(&ass.0),
                self.emit_rvalue(&ass.1)
            ),
            mir::StatementKind::SetDiscriminant {
                place,
                variant_index,
            } => format!(
                "(set-discriminant {} {})",
                self.emit_place(place),
                variant_index.index()
            ),
            mir::StatementKind::StorageLive(local) => format!("(storage-live _{})", local.index()),
            mir::StatementKind::StorageDead(local) => format!("(storage-dead _{})", local.index()),
            mir::StatementKind::Nop => format!("noop"),
            _ => "unknown-stmt".to_string(),
        }
    }

    fn emit_terminator(&self, term: &mir::Terminator) -> String {
        match &term.kind {
            mir::TerminatorKind::Goto { target } => format!("(goto bb{})", target.index()),
            mir::TerminatorKind::Resume => "resume".to_string(),
            mir::TerminatorKind::Abort => "abort".to_string(),
            mir::TerminatorKind::Return => "return".to_string(),
            mir::TerminatorKind::Unreachable => "unreachable".to_string(),
            mir::TerminatorKind::Drop {
                place,
                target,
                unwind,
            } => {
                let targets = match unwind {
                    None => format!("(bb{})", target.index()),
                    Some(unwind_bb) => format!("(bb{} bb{})", target.index(), unwind_bb.index()),
                };
                format!("(drop {} {targets})", self.emit_place(place))
            }
            mir::TerminatorKind::DropAndReplace {
                place,
                value,
                target,
                unwind,
            } => {
                let targets = match unwind {
                    None => format!("(bb{})", target.index()),
                    Some(unwind_bb) => format!("(bb{} bb{})", target.index(), unwind_bb.index()),
                };
                format!(
                    "(drop-and-replace {} {} {targets})",
                    self.emit_place(place),
                    self.emit_operand(value)
                )
            }
            mir::TerminatorKind::Call {
                func,
                args,
                destination,
                target,
                ..
            } => {
                let arg_ops = args
                    .iter()
                    .map(|arg| self.emit_operand(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!(
                    "(call {} {arg_ops} {} (bb{}))",
                    self.emit_operand(func),
                    self.emit_place(destination),
                    target.unwrap().index()
                )
            }
            _ => "unknown-term".to_string(),
        }
    }

    fn emit_body(&self, body: &mir::Body<'tcx>) -> String {
        let locals = body
            .local_decls
            .iter_enumerated()
            .map(|(local, decl)| {
                let mutability = match decl.mutability {
                    mir::Mutability::Not => "()",
                    mir::Mutability::Mut => "mut",
                };
                format!(
                    "(_{} {} {mutability})",
                    local.index(),
                    self.emit_ty(decl.ty)
                )
            })
            .intersperse("\n   ".to_string())
            .collect::<String>();

        let blocks = body
            .basic_blocks()
            .iter_enumerated()
            .map(|(bb_id, bb_data)| {
                let stmts = bb_data
                    .statements
                    .iter()
                    .map(|stmt| self.emit_statement(stmt))
                    .intersperse("\n      ".to_string())
                    .collect::<String>();

                let term = self.emit_terminator(bb_data.terminator());
                format!(
                    "(bb{} {{\n     ({stmts})\n     {term}\n   }})",
                    bb_id.index()
                )
            })
            .intersperse("\n   ".to_string())
            .collect::<String>();

        // TODO: generate KindedVarIds
        format!("(∃ () {{\n  ({locals})\n\n  ({blocks})\n}})")
    }

    fn emit_bound_var(&self, bound_var: ty::BoundVariableKind) -> String {
        match bound_var {
            ty::BoundVariableKind::Region(br_kind) => match br_kind {
                ty::BoundRegionKind::BrNamed(_, name) => format!("(lifetime {name})"),
                _ => format!("(lifetime {br_kind:?})"),
            },
            ty::BoundVariableKind::Ty(bt_kind) => format!("(type {bt_kind:?})"),
            _ => unimplemented!(),
        }
    }

    fn emit_where_clause(&self, pred: &ty::Predicate<'tcx>) -> String {
        let clause = match pred.kind().skip_binder() {
            ty::PredicateKind::Trait(trait_pred) => {
                let trait_name = self.tcx.def_path_str(trait_pred.def_id());
                let self_ty = self.emit_ty(trait_pred.self_ty());

                let params = trait_pred
                    .trait_ref
                    .substs
                    .iter()
                    .skip(1)
                    .map(|arg| self.emit_generic_arg(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!("({self_ty} : {trait_name} ({params}))")
            }
            ty::PredicateKind::RegionOutlives(outlives_pred) => {
                format!(
                    "({} : {}))",
                    self.emit_lifetime(outlives_pred.0),
                    self.emit_lifetime(outlives_pred.1)
                )
            }
            ty::PredicateKind::TypeOutlives(outlives_pred) => {
                format!(
                    "({} : {}))",
                    self.emit_ty(outlives_pred.0),
                    self.emit_lifetime(outlives_pred.1)
                )
            }
            ty::PredicateKind::Projection(proj_pred) => {
                let assoc_item = self
                    .tcx
                    .associated_item(proj_pred.projection_ty.item_def_id);
                let trait_name = self.tcx.def_path_str(assoc_item.container.id());

                let alias_params = proj_pred
                    .projection_ty
                    .substs
                    .iter()
                    .map(|arg| self.emit_generic_arg(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let rhs_ty = proj_pred.term.ty().unwrap_or_else(|| unimplemented!());

                format!(
                    "((alias-ty ({trait_name} {}) {alias_params}) == {})",
                    assoc_item.name,
                    self.emit_ty(rhs_ty),
                )
            }
            _ => "unknown-predicate".to_string(),
        };

        if pred.kind().bound_vars().is_empty() {
            clause
        } else {
            let vars = pred
                .kind()
                .bound_vars()
                .iter()
                .map(|var| self.emit_bound_var(var))
                .intersperse(" ".to_string())
                .collect::<String>();

            format!("(∀ ({vars}) {clause})")
        }
    }

    fn emit_item_decl(&self, item: &rustc_hir::Item) -> String {
        let name = item.ident;
        match item.kind {
            rustc_hir::ItemKind::Static(_, _, _) => {
                let ty = self.tcx.type_of(item.def_id);
                let ty_str = self.emit_ty(ty);

                let body = self
                    .tcx
                    .mir_built(ty::WithOptConstParam::unknown(item.def_id))
                    .borrow();
                let body_str = self.emit_body(&*body);

                format!("(static {name} () where () : {ty_str} = {body_str})")
            }
            rustc_hir::ItemKind::Fn(_, _, _) => {
                let generics = self.tcx.generics_of(item.def_id);

                let vars_early = generics.params.iter().map(|param| {
                    let kind = match param.kind {
                        ty::GenericParamDefKind::Lifetime => "lifetime",
                        ty::GenericParamDefKind::Type { .. } => "type",
                        _ => unimplemented!(),
                    };
                    format!("({kind} {})", param.name)
                });

                let sig = self.tcx.fn_sig(item.def_id);
                let vars_late = sig.bound_vars().iter().map(|var| self.emit_bound_var(var));

                let vars = vars_early
                    .chain(vars_late)
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let inputs = sig
                    .skip_binder()
                    .inputs()
                    .iter()
                    .map(|ty| self.emit_ty(*ty))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let output = self.emit_ty(sig.skip_binder().output());

                let predicates = self.tcx.predicates_of(item.def_id).predicates;
                let where_clauses = predicates
                    .iter()
                    .map(|(pred, _)| self.emit_where_clause(pred))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let body = self
                    .tcx
                    .mir_built(ty::WithOptConstParam::unknown(item.def_id))
                    .borrow();
                let body_str = self.emit_body(&*body);

                format!("(fn {name} ({vars}) ({inputs}) -> {output}\n  where ({where_clauses})\n  {body_str})")
            }
            _ => format!("(unknown-item {name:?})"),
        }
    }

    fn emit_user_ty(&self, ty: Ty<'tcx>) -> String {
        match ty.kind() {
            ty::TyKind::Bool => "bool".into(),
            ty::TyKind::Int(int_ty) => int_ty.name_str().into(),
            ty::TyKind::Uint(uint_ty) => uint_ty.name_str().into(),
            ty::TyKind::Adt(adt_def, substs) => {
                let def_path = self.tcx.def_path_str(adt_def.did());
                let substs_str = substs
                    .iter()
                    .map(|arg| self.emit_generic_arg(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!("({def_path} {substs_str})")
            }
            ty::TyKind::Ref(lt, ty, is_mut) => {
                let lifetime_str = self.emit_lifetime(*lt);
                let ty_str = self.emit_user_ty(*ty);
                match is_mut {
                    rustc_hir::Mutability::Not => format!("(& {lifetime_str} {ty_str})"),
                    rustc_hir::Mutability::Mut => format!("(&mut {lifetime_str} {ty_str})"),
                }
            }
            ty::TyKind::FnDef(_, _) => todo!(),
            ty::TyKind::FnPtr(fn_sig) => {
                let inputs = fn_sig
                    .skip_binder()
                    .inputs()
                    .iter()
                    .map(|ty| self.emit_user_ty(*ty))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let output = self.emit_user_ty(fn_sig.skip_binder().output());
                let fn_ty = format!("(fn ({inputs}) -> {output})");

                if fn_sig.bound_vars().is_empty() {
                    fn_ty
                } else {
                    let vars = fn_sig
                        .bound_vars()
                        .iter()
                        .map(|var| self.emit_bound_var(var))
                        .intersperse(" ".to_string())
                        .collect::<String>();

                    format!("(for ({vars}) {fn_ty})")
                }
            }
            ty::TyKind::Tuple(substs) => {
                if substs.is_empty() {
                    "()".to_string()
                } else {
                    let substs_str = substs
                        .into_iter()
                        .map(|ty| self.emit_user_ty(ty))
                        .intersperse(" ".to_string())
                        .collect::<String>();

                    format!("(tuple {substs_str})")
                }
            }
            ty::TyKind::Projection(projection_ty) => {
                let projection_ty: &ty::ProjectionTy<'tcx> = projection_ty;

                let self_ty = self.emit_user_ty(projection_ty.self_ty());
                let assoc_item = self.tcx.associated_item(projection_ty.item_def_id);
                let trait_def_id = assoc_item.container.id();
                let trait_name = self.tcx.def_path_str(trait_def_id);
                let trait_generics = self.tcx.generics_of(trait_def_id);

                let trait_params = projection_ty
                    .substs
                    .iter()
                    .take(trait_generics.count())
                    .skip(1)
                    .map(|arg| self.emit_generic_arg(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                let assoc_params = projection_ty
                    .substs
                    .iter()
                    .skip(trait_generics.count())
                    .map(|arg| self.emit_generic_arg(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!(
                    "(< {self_ty} as {trait_name}[{trait_params}] > :: {}[{assoc_params}])",
                    assoc_item.name,
                )
            }
            ty::TyKind::Param(param_ty) => format!("{}", param_ty.name),
            _ => format!("(unknown-ty {ty:?})"),
        }
    }

    fn emit_ty(&self, ty: Ty<'tcx>) -> String {
        format!("(user-ty {})", self.emit_user_ty(ty))
    }

    fn emit_generic_arg(&self, generic_arg: ty::subst::GenericArg<'tcx>) -> String {
        match generic_arg.unpack() {
            ty::subst::GenericArgKind::Lifetime(lt) => self.emit_lifetime(lt),
            ty::subst::GenericArgKind::Type(ty) => self.emit_user_ty(ty),
            ty::subst::GenericArgKind::Const(_) => unimplemented!(),
        }
    }

    fn emit_lifetime(&self, lifetime: ty::Region<'tcx>) -> String {
        match lifetime.kind() {
            ty::RegionKind::ReStatic => "static".to_string(),
            ty::RegionKind::ReEarlyBound(re) => format!("{}", re.name),
            ty::RegionKind::ReLateBound(
                _,
                ty::BoundRegion {
                    kind: ty::BoundRegionKind::BrNamed(_, name),
                    ..
                },
            ) => format!("{name}"),
            ty::RegionKind::ReErased => "?".to_string(),
            _ => format!("(unknown-lt {lifetime:?})"),
        }
    }
}

fn main() {
    let out = process::Command::new("rustc")
        .arg("--print=sysroot")
        .current_dir(".")
        .output()
        .unwrap();
    let sysroot = str::from_utf8(&out.stdout).unwrap().trim();
    let config = rustc_interface::Config {
        // Command line options
        opts: config::Options {
            maybe_sysroot: Some(path::PathBuf::from(sysroot)),
            debugging_opts: config::DebuggingOptions {
                dump_mir: Some("all".to_string()),
                ..config::DebuggingOptions::default()
            },
            ..config::Options::default()
        },
        // cfg! configuration in addition to the default ones
        crate_cfg: FxHashSet::default(), // FxHashSet<(String, Option<String>)>
        crate_check_cfg: CheckCfg::default(), // CheckCfg
        input: config::Input::Str {
            name: source_map::FileName::Custom("main.rs".into()),
            input: r#"
static UNIT: &'static &'static () = &&();

fn foo<'a, 'b, T>(_: &'a &'b (), v: &'b T) -> &'a T {
    v
}

fn bad<'a, T>(x: &'a T) -> &'static T {
    let f: fn(_, &'a T) -> &'static T = foo;
    f(UNIT, x)
}

fn main() {
    let value = {
        let data = 22;
        bad(&data)
    };
    let _ = *value;
}
"#
            .into(),
        },
        input_path: None,  // Option<PathBuf>
        output_dir: None,  // Option<PathBuf>
        output_file: None, // Option<PathBuf>
        file_loader: None, // Option<Box<dyn FileLoader + Send + Sync>>
        diagnostic_output: rustc_session::DiagnosticOutput::Default,
        lint_caps: FxHashMap::default(), // FxHashMap<lint::LintId, lint::Level>
        // This is a callback from the driver that is called when [`ParseSess`] is created.
        parse_sess_created: None, //Option<Box<dyn FnOnce(&mut ParseSess) + Send>>
        // This is a callback from the driver that is called when we're registering lints;
        // it is called during plugin registration when we have the LintStore in a non-shared state.
        //
        // Note that if you find a Some here you probably want to call that function in the new
        // function being registered.
        register_lints: None, // Option<Box<dyn Fn(&Session, &mut LintStore) + Send + Sync>>
        // This is a callback from the driver that is called just after we have populated
        // the list of queries.
        //
        // The second parameter is local providers and the third parameter is external providers.
        override_queries: None, // Option<fn(&Session, &mut ty::query::Providers<'_>, &mut ty::query::Providers<'_>)>
        // Registry of diagnostics codes.
        registry: registry::Registry::new(&rustc_error_codes::DIAGNOSTICS),
        make_codegen_backend: None,
    };
    rustc_interface::run_compiler(config, |compiler| {
        compiler.enter(|queries| {
            // Analyze the program and inspect the types of definitions.
            queries.global_ctxt().unwrap().take().enter(|tcx| {
                let gen = FormalityGen { tcx };
                println!("{}", gen.generate_decls());
            })
        });
    });
}
