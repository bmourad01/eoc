# eoc

Implementation of a compiler for a subset of Racket, based on the book [Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation).

## Requirements

- OCaml (>= 4.08)
- nasm (>= 2.15.05)
- gcc (>= 9.3.0)
- a POSIX-compliant shell

Additionally, the following OCaml packages are required:

- core_kernel (>= 0.14.0)
- dune (>= 2.8.0)
- menhir
- ocamlgraph (>= 2.0.0)
- ppx_compare
- ppx_hash
- ppx_let
- ppx_sexp_conv
