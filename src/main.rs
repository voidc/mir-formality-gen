#![feature(rustc_private)]
#![feature(iter_intersperse)]

// rustup component add rustc-dev llvm-tools-preview
// version: 1.63.0-nightly (5435ed691 2022-06-07)

extern crate rustc_error_codes;
extern crate rustc_errors;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;

use std::{env, path, process, str};

use rustc_errors::registry;
use rustc_hash::{FxHashMap, FxHashSet};
use rustc_session::config::{self, CheckCfg};

mod gen;
mod renumber_mir;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    let emit_let = args.iter().any(|arg| arg == "--let");
    let input_path = args
        .iter()
        .find(|arg| !arg.starts_with("--"))
        .expect("Specify input path");

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
        input: config::Input::File(path::PathBuf::from(input_path)),
        input_path: Some(path::PathBuf::from(input_path)), // Option<PathBuf>
        output_dir: None,                                  // Option<PathBuf>
        output_file: None,                                 // Option<PathBuf>
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
                let gen = gen::FormalityGen::new(tcx);
                if emit_let {
                    println!("{}", gen.emit_local_decls_in_let());
                } else {
                    println!("{}", gen.emit_local_crate());
                }
            })
        });
    });
}
