use rustc_middle::ty;

use crate::gen::FormalityGen;

impl<'tcx> FormalityGen<'tcx> {
    pub fn emit_local_crate(&self) -> String {
        let crate_name = self.tcx.crate_name(rustc_hir::def_id::LOCAL_CRATE);

        let crate_items = self
            .tcx
            .hir()
            .items()
            .map(|crate_item_id| {
                let item = self.tcx.hir().item(crate_item_id);
                match item.kind {
                    rustc_hir::ItemKind::Static(_, _, _) => {
                        self.emit_static_decl(item.ident, item.def_id)
                    }
                    rustc_hir::ItemKind::Fn(_, _, _) => {
                        self.emit_fn_decl(item.ident, item.def_id, true)
                    }
                    rustc_hir::ItemKind::Trait(_, _, _, _, trait_items) => {
                        self.emit_trait_decl(item.ident, item.def_id, trait_items)
                    }
                    rustc_hir::ItemKind::Impl(impl_item) => {
                        self.emit_trait_impl_decl(item.def_id, impl_item.items)
                    }
                    rustc_hir::ItemKind::Struct(_, _)
                    | rustc_hir::ItemKind::Enum(_, _)
                    | rustc_hir::ItemKind::Union(_, _) => {
                        self.emit_adt_decl(item.ident, item.def_id)
                    }
                    _ => format!("(unknown-item {:?})", item.ident),
                }
            })
            .intersperse("\n\n".to_string())
            .collect::<String>();

        format!("({crate_name} (crate [\n{crate_items}]))")
    }

    pub fn emit_local_decls_in_let(&self) -> String {
        let mut item_names = Vec::new();
        let mut item_decls = Vec::new();

        for crate_item_id in self.tcx.hir().items() {
            let item = self.tcx.hir().item(crate_item_id);
            let (item_name, item_decl) = match item.kind {
                rustc_hir::ItemKind::Static(_, _, _) => (
                    format!("StaticDecl_{}", item.ident),
                    self.emit_static_decl(item.ident, item.def_id),
                ),
                rustc_hir::ItemKind::Fn(_, _, _) => (
                    format!("FnDecl_{}", item.ident),
                    self.emit_fn_decl(item.ident, item.def_id, true),
                ),
                rustc_hir::ItemKind::Trait(_, _, _, _, trait_items) => (
                    format!("TraitDecl_{}", item.ident),
                    self.emit_trait_decl(item.ident, item.def_id, trait_items),
                ),
                rustc_hir::ItemKind::Impl(impl_item) => (
                    format!("TraitImplDecl_{}", item.ident),
                    self.emit_trait_impl_decl(item.def_id, impl_item.items),
                ),
                rustc_hir::ItemKind::Struct(_, _)
                | rustc_hir::ItemKind::Enum(_, _)
                | rustc_hir::ItemKind::Union(_, _) => (
                    format!("AdtDecl_{}", item.ident),
                    self.emit_adt_decl(item.ident, item.def_id),
                ),
                _ => (
                    format!("UnknownDecl_{}", item.ident),
                    format!("(unknown-item {:?})", item.ident),
                ),
            };

            item_decls.push(format!("({item_name} (term {item_decl}))"));
            item_names.push(item_name);
        }

        let item_decls = item_decls
            .into_iter()
            .intersperse("\n ".to_string())
            .collect::<String>();

        let item_names = item_names
            .into_iter()
            .intersperse(" ".to_string())
            .collect::<String>();

        let crate_name = self.tcx.crate_name(rustc_hir::def_id::LOCAL_CRATE);
        let crate_decl = format!("(CrateDecl (term ({crate_name} (crate [{item_names}]))))");

        format!("(redex-let*\n formality-mir\n [{item_decls}\n {crate_decl}\n ])")
    }

    fn emit_adt_decl(
        &self,
        name: rustc_span::symbol::Ident,
        def_id: rustc_span::def_id::LocalDefId,
    ) -> String {
        let adt_def: ty::AdtDef<'tcx> = self.tcx.adt_def(def_id);
        let adt_kind = match adt_def.adt_kind() {
            ty::AdtKind::Struct => "struct",
            ty::AdtKind::Union => "union",
            ty::AdtKind::Enum => "enum",
        };

        let vars = self.emit_generics(def_id);
        let where_clauses = self.emit_where_clauses(def_id);

        let variants = adt_def
            .variants()
            .iter()
            .map(|variant| {
                let fields = variant
                    .fields
                    .iter()
                    .map(|field| {
                        let field_ty = self.emit_ty(self.tcx.type_of(field.did));
                        format!("({} {field_ty})", field.name)
                    })
                    .intersperse(" ".to_string())
                    .collect::<String>();

                format!("({} [{fields}])", variant.name)
            })
            .intersperse("\n  ".to_string())
            .collect::<String>();

        format!("({adt_kind} {name}[{vars}] where [{where_clauses}] {{\n  {variants}\n}})")
    }

    fn emit_trait_decl(
        &self,
        name: rustc_span::symbol::Ident,
        def_id: rustc_span::def_id::LocalDefId,
        trait_items: &[rustc_hir::TraitItemRef],
    ) -> String {
        let vars = self.emit_generics(def_id);
        let where_clauses = self.emit_where_clauses(def_id);

        let trait_items = trait_items
            .iter()
            .map(|trait_item| match trait_item.kind {
                rustc_hir::AssocItemKind::Fn { .. } => self.emit_fn_decl(
                    trait_item.ident,
                    trait_item.id.def_id,
                    trait_item.defaultness.has_value(),
                ),
                rustc_hir::AssocItemKind::Type => {
                    self.emit_assoc_ty_decl(trait_item.ident, trait_item.id.def_id)
                }
                rustc_hir::AssocItemKind::Const => unimplemented!(),
            })
            .intersperse("\n  ".to_string())
            .collect::<String>();

        format!("(trait {name}[{vars}] where [{where_clauses}] {{\n  {trait_items}\n}})")
    }

    fn emit_assoc_ty_decl(
        &self,
        name: rustc_span::symbol::Ident,
        def_id: rustc_span::def_id::LocalDefId,
    ) -> String {
        let generics = self.emit_generics(def_id);
        let where_clauses = self.emit_where_clauses(def_id);

        let bounds: &'tcx ty::List<ty::Predicate<'tcx>> = self.tcx.item_bounds(def_id);
        let bounds = bounds
            .iter()
            .map(|pred| self.emit_where_clause(&pred, Some("T".to_string())))
            .intersperse(" ".to_string())
            .collect::<String>();

        format!("(type {name}[{generics}] (: (type T) [{bounds}]) where [{where_clauses}])")
    }

    fn emit_trait_impl_decl(
        &self,
        def_id: rustc_span::def_id::LocalDefId,
        impl_items: &[rustc_hir::ImplItemRef],
    ) -> String {
        let impl_vars = self.emit_generics(def_id);
        let trait_ref: ty::TraitRef<'tcx> = self
            .tcx
            .impl_trait_ref(def_id)
            .unwrap_or_else(|| unimplemented!("inherent impl"));
        let trait_ref = self.emit_trait_ref(trait_ref);
        let where_clauses = self.emit_where_clauses(def_id);

        let impl_items = impl_items
            .iter()
            .map(|impl_item| match impl_item.kind {
                rustc_hir::AssocItemKind::Fn { .. } => {
                    self.emit_fn_decl(impl_item.ident, impl_item.id.def_id, true)
                }
                rustc_hir::AssocItemKind::Type => {
                    self.emit_assoc_ty_value_decl(impl_item.ident, impl_item.id.def_id)
                }
                rustc_hir::AssocItemKind::Const => unimplemented!(),
            })
            .intersperse("\n  ".to_string())
            .collect::<String>();

        format!("(impl[{impl_vars}] {trait_ref} where [{where_clauses}] {{\n  {impl_items}\n}})")
    }

    fn emit_assoc_ty_value_decl(
        &self,
        name: rustc_span::symbol::Ident,
        def_id: rustc_span::def_id::LocalDefId,
    ) -> String {
        let generics = self.emit_generics(def_id);
        let where_clauses = self.emit_where_clauses(def_id);
        let alias_ty = self.emit_ty(self.tcx.type_of(def_id));

        format!("(type {name}[{generics}] = {alias_ty} where [{where_clauses}])")
    }

    fn emit_fn_decl(
        &self,
        name: rustc_span::symbol::Ident,
        def_id: rustc_span::def_id::LocalDefId,
        has_body: bool,
    ) -> String {
        let generics: &'tcx ty::Generics = self.tcx.generics_of(def_id);

        let vars_early = generics
            .params
            .iter()
            .map(|param| self.emit_generic_param(param));

        let sig = self.tcx.fn_sig(def_id);
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
        let where_clauses = self.emit_where_clauses(def_id);

        if has_body {
            let body = self
                .tcx
                .mir_built(ty::WithOptConstParam::unknown(def_id))
                .borrow();

            let body_str = self.emit_body(&*body);
            format!(
                "(fn {name}[{vars}] ({inputs}) -> {output}\n where [{where_clauses}]\n {body_str})"
            )
        } else {
            format!(
                "(fn {name}[{vars}] ({inputs}) -> {output} where [{where_clauses}] trusted-fn-body)"
            )
        }
    }

    fn emit_static_decl(
        &self,
        name: rustc_span::symbol::Ident,
        def_id: rustc_span::def_id::LocalDefId,
    ) -> String {
        let ty = self.tcx.type_of(def_id);
        let ty_str = self.emit_ty(ty);

        let body = self
            .tcx
            .mir_built(ty::WithOptConstParam::unknown(def_id))
            .borrow();
        let body_str = self.emit_body(&*body);

        format!("(static {name}[] where () : {ty_str} =\n {body_str})")
    }

    fn emit_generics(&self, def_id: rustc_span::def_id::LocalDefId) -> String {
        let generics: &'tcx ty::Generics = self.tcx.generics_of(def_id);
        generics
            .params
            .iter()
            .map(|param| self.emit_generic_param(param))
            .intersperse(" ".to_string())
            .collect::<String>()
    }

    fn emit_where_clauses(&self, def_id: rustc_span::def_id::LocalDefId) -> String {
        let predicates = self.tcx.predicates_of(def_id).predicates;
        predicates
            .iter()
            .map(|(pred, _)| self.emit_where_clause(pred, None))
            .intersperse(" ".to_string())
            .collect::<String>()
    }
}
