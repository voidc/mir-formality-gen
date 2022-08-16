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

    pub fn generate(&self) -> String {
        let program = self.emit_program();
        format!(
            r#"
#lang racket
(require redex/reduction-semantics
         "src/ty/user-ty.rkt"
         "src/rust/grammar.rkt"
         "src/rust/prove.rkt"
         )

(module+ test
  (redex-let*
   formality-rust

   [(Rust/Program (term {program}))
    ]

   (test-equal #t (term (rust:is-program-ok Rust/Program)))
   )
  )"#
        )
    }

    pub fn emit_ident(&self, ident: &Symbol) -> String {
        // Racket special characters: ( ) [ ] { } " , ' ` ; # | \
        ident.as_str().replace('\'', Self::LIFETIME_MARKER)
    }
}
