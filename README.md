# Nocky

A strongly- and statically-typed language, inspired by Rust.

# Usage
```sh
  nockyc [options] <source>
  Options:
  -lexer: Run the lexer on <source>, then exit
  -parser: Run the parser on <source>, then reconstruct the program from the AST and print to stdout
```
# Running Tests

Requires `cargo` and `insta` (`cargo install cargo-insta`). Run `cargo insta test` to run all tests.
