use rustc_middle::ty::{self};

use crate::gen::FormalityGen;

impl<'tcx> FormalityGen<'tcx> {
    pub fn generate_decls(&self) -> String {
        let mut res = String::new();
        for id in self.tcx.hir().items() {
            let item = self.tcx.hir().item(id);
            res.push_str(&self.emit_item_decl(item));
            res.push_str("\n\n");
        }
        res
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
}
