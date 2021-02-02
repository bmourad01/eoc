# eoc

Implementation of a compiler from a subset of Typed Racket to AMD64, based on the book [Essentials of Compilation](https://github.com/IUCompilerCourse/Essentials-of-Compilation) by Jeremy Siek.

# Requirements

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

# Instructions

For building the project, run `dune build`. To compile a program, run `./compile <program>.rkt`. To execute the compiled program, run `./output/<program>`.

# Target Machine

First of all, the compiler targets AMD64 (otherwise known as x86-64 to mindless plebians) and follows the System V ABI.

Given this, the output assembly programs should work fine on a corresponding Linux system. BSD and macOS systems have not been tested, so your mileage may vary. Windows is a non-starter since it follows a non-standard ABI.

# Features Implemented

- Integers, booleans
- Basic arithmetic and bitwise operations
- Let-binding
- If-then-else expressions
- Register allocation with move biasing
- Tuples (via `vector`) & garbage collection
- Top-level functions via `define`
- Anonymous functions via `lambda`
- Loops (via `while`), sequencing (via `begin`, `when`, and `unless`), and assignment (via `set!`)

# Why are you using `nasm`?

Partly for familiarity, partly because it resembles Intel syntax for x86 assembly. I find AT&T syntax to be very ugly, and the mnemonics it uses conflict with those found in the official Intel manuals.

I'm aware that GNU `as` can allow for Intel syntax. I just haven't implemented such a backend (yet).
