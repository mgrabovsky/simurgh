# Simurgh

One day in the distant future, Simurgh aims to be a total dependently typed
programming language. It is no more than a skeleton as of August 2019. Currently,
there is an absolutely minimal, dependently typed core lambda calculus with lambda
abstractions, Pi types, `let` constructs and a single type universe `Set : Set`.

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
    currently the evaluator and parser
-   `notes/` contains various remarks on the development, the theory around it and
    some tools used throughout

## Roadmap/Wishlist

- [x] Modern library for binding syntax – [Unbound](https://hackage.haskell.org/package/unbound-generics)
- [ ] Continuous integration with Travis
- [ ] Modern pretty-printing – [Hughes/PJ](https://hackage.haskell.org/package/pretty),
  [Wadler/Leijen](https://hackage.haskell.org/package/ansi-wl-pprint),
  or [mainland-pretty](https://hackage.haskell.org/package/mainland-pretty)
- [ ] Tests
- [ ] Improve parsing and error reporting. Look into
  [Trifecta](https://hackage.haskell.org/package/trifecta)
- [ ] See if lenses can be leveraged anywhere
- [ ] Pattern matching
- [ ] \(Mutually) inductive data types
- [ ] Module system, global definitions, `let`s
- [ ] Play around with equality (intensional/extensional/observational)
- [ ] Type universe hierarchy; subtyping
- [ ] Recursive functions, fixpoints
- [ ] Implicit arguments; elaboration à la Lean
- [ ] More sophisticated syntax as part of higher-level languages which transform
  into the low-level core language
- [ ] First-class support for monads or algebraic effects

## References

Learning resources, hints, etc.:

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

