use std::env;
use std::path::PathBuf;

fn main() {
    build_c_library();
    build_bindings();
}

fn build_c_library() {
    let mut build = cc::Build::new();

    build
        .cpp(true)
        .file("c_src/image.cpp")
        .warnings(true)
        .extra_warnings(true)
        .std("c++20");

    let config = pkg_config::Config::new();
    let library = config
        .probe("Qt6Core Qt6Gui")
        .or_else(|_| config.probe("Qt5Core Qt5Gui"))
        .unwrap();

    // For some reason, pkg_config doesn't do this.
    for path in library.include_paths {
        build.include(path);
    }

    build.compile("image");

    // This is automatically done by pkg_config, but it puts the
    // libraries *before* -limage, and since that's static, it fails.
    // Manually adding them afterward is fine.
    for lib in library.libs {
        println!("cargo:rustc-link-lib={lib}");
    }

    println!("cargo:rerun-if-changed=c_src/image.cpp");
    println!("cargo:rerun-if-changed=c_src/image.h");
}

fn build_bindings() {
    println!("cargo:rerun-if-changed=c_src/image.h");

    let bindings = bindgen::Builder::default()
        .header("c_src/image.h")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
