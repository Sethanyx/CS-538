# Functional Language Interpreter

A functional programming language interpreter written in Haskell. This project implements a complete frontend and backend for a functional language subset (similar to ML or Haskell), featuring a Parsec-based parser, a robust Hindley-Milner type inference system, and an evaluator that supports closures and pattern matching.

## Features

* **Interactive REPL**: A command-line environment powered by `haskeline` and `repline` with command auto-completion and history.
* **Type Inference**: Full implementation of the Hindley-Milner (HM) type system, supporting polymorphic types (`Forall`), unification, and automatic type deduction.
* **Data Types**: 
  * Primitive types: `Int` and `Bool`.
  * Arrays/Lists: Support for array literals (e.g., `[1, 2, 3]`), the `Cons` operator (`:`), and the `Concat` operator (`++`).
* **Functions & Recursion**: Support for first-class functions (lambdas) and recursive function definitions using `let rec`.
* **Pattern Matching**: Advanced pattern matching in function arguments and `let` bindings, including literal matching, empty list matching (`[]`), and list destructuring (`x:xs`).
* **Pretty Printing**: Clean and readable formatting for AST nodes, types, and evaluation results.

## Getting Started

Make sure you have the Glasgow Haskell Compiler (GHC) installed, along with the necessary dependencies (`parsec`, `mtl`, `haskeline`, `repline`).

Run the interpreter directly:

```bash
runhaskell Main.hs