pub fn main() {
    println!("cargo::rerun-if-changed=tests/resources");
    println!("cargo::rerun-if-env-changed=BASE_TEST_DIR");
}
