use rustc_middle::ty;

use crate::gen::FormalityGen;

impl<'tcx> FormalityGen<'tcx> {
    pub fn emit_where_clause(
        &self,
        pred: &ty::Predicate<'tcx>,
        replace_self_ty: Option<String>,
    ) -> String {
        let clause = match pred.kind().skip_binder() {
            ty::PredicateKind::Trait(trait_pred) => {
                let trait_name = self.tcx.def_path_str(trait_pred.def_id());
                let self_ty = replace_self_ty.unwrap_or_else(|| self.emit_ty(trait_pred.self_ty()));

                let params = trait_pred
                    .trait_ref
                    .substs
                    .iter()
                    .skip(1)
                    .map(|arg| self.emit_generic_arg(arg))
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!("({self_ty} : {trait_name}[{params}])")
            }
            ty::PredicateKind::RegionOutlives(outlives_pred) => {
                assert!(replace_self_ty.is_none());
                format!(
                    "({} : {}))",
                    self.emit_lifetime(outlives_pred.0),
                    self.emit_lifetime(outlives_pred.1)
                )
            }
            ty::PredicateKind::TypeOutlives(outlives_pred) => {
                let self_ty = replace_self_ty.unwrap_or_else(|| self.emit_ty(outlives_pred.0));
                format!("({} : {}))", self_ty, self.emit_lifetime(outlives_pred.1))
            }
            ty::PredicateKind::Projection(proj_pred) => {
                assert!(replace_self_ty.is_none());
                let assoc_item: &ty::AssocItem = self
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
                    "((alias-ty ({trait_name} {})[{alias_params}]) == {})",
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

    fn emit_user_ty(&self, ty: ty::Ty<'tcx>) -> String {
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

    pub fn emit_ty(&self, ty: ty::Ty<'tcx>) -> String {
        format!("(user-ty {})", self.emit_user_ty(ty))
    }

    pub fn emit_trait_ref(&self, trait_ref: ty::TraitRef<'tcx>) -> String {
        let name = self.tcx.def_path_str(trait_ref.def_id);
        let args = trait_ref
            .substs
            .iter()
            .map(|arg| self.emit_generic_arg(arg))
            .intersperse(" ".to_string())
            .collect::<String>();

        format!("({name}[{args}])")
    }

    fn emit_generic_arg(&self, generic_arg: ty::subst::GenericArg<'tcx>) -> String {
        match generic_arg.unpack() {
            ty::subst::GenericArgKind::Lifetime(lt) => self.emit_lifetime(lt),
            ty::subst::GenericArgKind::Type(ty) => self.emit_user_ty(ty),
            ty::subst::GenericArgKind::Const(_) => unimplemented!(),
        }
    }

    pub fn emit_generic_param(&self, param: &'tcx ty::GenericParamDef) -> String {
        let kind = match param.kind {
            ty::GenericParamDefKind::Lifetime => "lifetime",
            ty::GenericParamDefKind::Type { .. } => "type",
            _ => unimplemented!(),
        };
        format!("({kind} {})", param.name)
    }

    pub fn emit_bound_var(&self, bound_var: ty::BoundVariableKind) -> String {
        match bound_var {
            ty::BoundVariableKind::Region(br_kind) => match br_kind {
                ty::BoundRegionKind::BrNamed(_, name) => format!("(lifetime {name})"),
                ty::BoundRegionKind::BrAnon(idx) => format!("(lifetime '{idx})"),
                _ => format!("(lifetime {br_kind:?})"),
            },
            ty::BoundVariableKind::Ty(bt_kind) => format!("(type {bt_kind:?})"),
            _ => unimplemented!(),
        }
    }

    pub fn emit_lifetime(&self, lifetime: ty::Region<'tcx>) -> String {
        match lifetime.kind() {
            ty::RegionKind::ReStatic => "static".to_string(),
            ty::RegionKind::ReEarlyBound(re) => format!("{}", re.name),
            ty::RegionKind::ReLateBound(_, bound_region) => match bound_region.kind {
                ty::BoundRegionKind::BrNamed(_, name) => format!("{name}"),
                ty::BoundRegionKind::BrAnon(idx) => format!("'{idx}"),
                _ => format!("{:?}", bound_region.kind),
            },
            ty::RegionKind::ReErased => "?".to_string(),
            _ => format!("(unknown-lt {lifetime:?})"),
        }
    }
}
