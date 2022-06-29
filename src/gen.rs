use rustc_middle::ty::TyCtxt;
use rustc_span::Symbol;

mod decl;
mod mir;
mod ty;

pub struct FormalityGen<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}

impl<'tcx> FormalityGen<'tcx> {
    const LIFETIME_MARKER: &'static str = "%";

    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        FormalityGen { tcx }
    }

    pub fn emit_ident(&self, ident: &Symbol) -> String {
        // Racket special characters: ( ) [ ] { } " , ' ` ; # | \
        ident.as_str().replace('\'', Self::LIFETIME_MARKER)
    }
}
