# Simurgh

One day in the distant future, Simurgh aims to be a (toy) total dependently typed
programming language and a (toy) theorem prover. As of August 2019, it is no more
than a skeleton. Currently, there is an absolutely minimal dependently typed core
lambda calculus with lambda abstractions, Pi types, `let` constructs and a single
type universe `Set : Set`.

## Motivation

I was amazed by the power and capabilities of the "big trinity" of CC-rooted theorem
provers, [Coq](https://coq.inria.fr/),
[Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php) and
[Idris](https://www.idris-lang.org/). I lived a happy, complacent certified life for
some time untill [F*](https://www.fstar-lang.org/) and
[Lean](https://leanprover.github.io/) appeared. They significantly changed my view of
theorem provers, their potential and especially their usability. Lean and F* have
shown that it is possible to improve the user experience of theorem proving and
certified programming much more than what I had presumed before.

## Directory structure

-   `src/Simurgh/` contains the language implementation
    +   `Syntax.hs` defines the abstract syntax type, telescopes and functions for
        working with them
    +   `Eval.hs` contains the evaluator with various reduction strategies
    +   `Typing.hs` implements the bidirectional type checker and conversion rules
    +   `Pretty.hs` has some facilities for pretty-printing the expressions of our
        core language
    +   `Parser.hs` implements a Parsec-based parser for the core language
-   the `app/` directory contains a simple REPL for evaluating and typing the
    expressions interactively
-   `test/` contains a few very rudimentary tests of some of the components,
    currently the evaluator and the parser
-   `notes/` contains various remarks on the development, the theory around it and
    some tools used throughout

## Interactive prompt

The `simurgh-exe` target provides a simple interactive prompt (or REPL) to test the
features of the language. If a valid term is entered on the command line, it is
evaluated and the result is printed out (no type checking of the input yet).

Currently, the following commands are supported:

-   `:help` will print the help message which currently lists the available commands,
    i.e. similarly to this very list.
-   `:quit` will terminate the REPL.
-   `:type <expr>` will infer and print the type of the given expression.

## Building

The project is built on the Haskell Tool Stack, thus the following sequence of
commands should suffice to build the library and the REPL executable:
```
$ git clone https://github.com/mgrabovsky/simurgh.git
$ cd simurgh
$ stack setup
$ stack build
```

To run the REPL upon building the package, you can simply invoke
```
$ stack exec simurgh-exe
```

## Roadmap/Wishlist

Regarding the language, listed roughly in ascending order of difficulty,
interestingness and distance in time:

- [x] Modern library for binding syntax – [Unbound](https://hackage.haskell.org/package/unbound-generics)
- [ ] Named definitions and axioms (think about opacity later)
- [ ] Type universe hierarchy; subtyping
- [ ] Impredicative, proof-irrelevant universe (à la `Prop`)?
- [ ] Module system or namespaces
- [ ] Opaque definitions
- [ ] \(Mutually) inductive data types
- [ ] Pattern matching
- [ ] Primitive recursive functions
- [ ] Structurally recursive functions
- [ ] Investigate more elaborate techniques for termination checking (sized types,
  etc.)
- [ ] Coinduction
- [ ] Induction-recursion and the kitchen sink
- [ ] Play around with notions of equality (intensional/extensional/observational)
- [ ] Investigate quotient types
- [ ] Introduce a higher-level user-facing language with a more usable syntax
  compiling into the into the low-level core language
- [ ] Implicit arguments, metavariables; elaboration à la Lean
- [ ] Records and typeclasses
- [ ] Implicit coercions
- [ ] First-class support for monads (do-notation) or algebraic effects and handlers
- [ ] Compilation to some real-world language, be it Haskell, Idris, Rust or C
- [ ] [LLVM](https://github.com/llvm-hs/llvm-hs/) compilation

Regarding infrastructure and libraries:

- [ ] Thorough tests, at least for the parser and type checker
- [ ] Continuous integration with Travis
- [ ] Modern pretty-printing – [Hughes/PJ](https://hackage.haskell.org/package/pretty),
  [Wadler/Leijen](https://hackage.haskell.org/package/ansi-wl-pprint)
  or [mainland-pretty](https://hackage.haskell.org/package/mainland-pretty)
- [ ] Improve parsing and error reporting; look into
  [Trifecta](https://hackage.haskell.org/package/trifecta)
- [ ] See if lenses can be leveraged anywhere

## References

Reference papers and books, learning resources, study materials, etc. will be
listed here.

-   Löh, A\., McBride, C., Swierstra, W.: [_Simply Easy! An Implementation of a Dependently Typed Lambda Calculus_](http://strictlypositive.org/Easy.pdf)
-   Altenkirch, T\., Danielsson, N. A., Löh, A., Oury, N.: [_ΠΣ: Dependent Types without the Sugar_](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf)
-   Abel, A\., Altenkirch, T.: [_A Partial Type Checking Algorithm for Type : Type_](http://www.cs.nott.ac.uk/~psztxa/publ/msfp08.pdf)
-   Altenkirch, T\., McBride, C.: [_Towards Observational Type Theory_](http://strictlypositive.org/ott.pdf)
-   Altenkirch, T\., McBride, C., Swierstra, W.: [_Observational Equality, Now!_](http://www.cs.nott.ac.uk/~psztxa/publ/obseqnow.pdf)
-   de Moura, L\., Avigad, J., Kong, S., Roux, C.: [_Elaboration in Dependent Type Theory_](http://www.contrib.andrew.cmu.edu/~avigad/Papers/constr.pdf)
-   de Moura et al\.: [_The Lean Theorem Prover (system description)_](https://leanprover.github.io/papers/system.pdf)
-   Danny Gratzer: [_Bidirectional Type Checkers for λ→ and λΠ_](https://jozefg.bitbucket.io/posts/2014-11-22-bidir.html)
-   Paulin-Mohring, C\.: [_Introduction to the Calculus of Inductive Constructions_](https://hal.inria.fr/hal-01094195/)
-   Eisenberg, R\. A.: [_Stitch: The Sound Type-Indexed Type Checker_](https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.pdf)

Also inspired by

-   Stephanie Weirich's [pi-forall](https://github.com/sweirich/pi-forall)
-   Mörtberg et al.'s [implementation of Cubical Type Theory](https://github.com/mortberg/cubicaltt)
-   Bauer at al.'s [Andromeda](https://andromedans.github.io/andromeda/)

## Licence

The code, documentation and other resources in this repository are provided under the
conditions of the [Blue Oak Model License 1.0.0](https://blueoakcouncil.org/license/1.0.0).

