# eoc

Implementation of a compiler from a subset of Racket to AMD64, based on the book [Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation).

## Requirements

- OCaml (>= 4.08)
- nasm (>= 2.15.05)
- a C99-compatible C compiler
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

## Instructions

For building the project, run `dune build`. To compile a program, run `./compile <program>.rkt`. To execute the compiled program, run `./output/<program>`.

## Features Implemented

- Integers, booleans
- Basic arithmetic and bitwise operations
- Let-binding
- If-then-else expressions
- Register allocation with move biasing
- Tuples (`vector`) & garbage collection
- Top-level functions via `define`
- Anonymous functions via `lambda`
