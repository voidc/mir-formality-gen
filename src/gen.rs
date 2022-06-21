use rustc_middle::ty::TyCtxt;

mod decl;
mod mir;
mod ty;

pub struct FormalityGen<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}
