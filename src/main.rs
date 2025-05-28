use std::path::PathBuf;
use std::process;
use clap::{App, Arg};
use golangx_compiler::compiler::{Compiler, CompilerOptions};

fn main() {
    let matches = App::new("GolangX Compiler")
        .version("1.0.0")
        .author("GolangX Team")
        .about("Compiles GolangX code to Golang")
        .arg(Arg::with_name("INPUT")
            .help("Input files or directories")
            .required(true)
            .multiple(true))
        .arg(Arg::with_name("output")
            .short("o")
            .long("output")
            .value_name("DIR")
            .help("Output directory")
            .takes_value(true))
        .arg(Arg::with_name("verbose")
            .short("v")
            .long("verbose")
            .help("Enable verbose output"))
        .arg(Arg::with_name("extension")
            .short("e")
            .long("extension")
            .value_name("EXT")
            .help("File extension for GolangX files (default: .gx)")
            .takes_value(true))
        .get_matches();
    
    let input_paths: Vec<PathBuf> = matches.values_of("INPUT")
        .unwrap()
        .map(PathBuf::from)
        .collect();
    
    let output_dir = matches.value_of("output")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("./out"));
    
    let verbose = matches.is_present("verbose");
    
    let extension = matches.value_of("extension")
        .unwrap_or(".gx")
        .to_string();
    
    let options = CompilerOptions {
        input_paths,
        output_dir,
        verbose,
        extension,
    };
    
    let mut compiler = Compiler::new(options);
    
    match compiler.compile() {
        Ok(report) => {
            println!("{}", report.summary());
            
            if verbose {
                println!("\n{}", report.detailed());
            }
            
            if !report.failed_files.is_empty() || !report.failed_directories.is_empty() {
                process::exit(1);
            }
        },
        Err(e) => {
            eprintln!("Compilation failed: {}", e);
            process::exit(1);
        }
    }
}
