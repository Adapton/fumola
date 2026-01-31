extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/lib/parser.lalrpop");
    println!("cargo:rerun-if-changed=build.rs");

    lalrpop::Configuration::new()
        .set_out_dir(std::env::var("OUT_DIR").unwrap())
        .process_file("src/lib/parser.lalrpop")
        .unwrap();
}
