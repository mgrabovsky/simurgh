# Simurgh

One day in the distant future, Simurgh aims to be a (toy) total dependently typed
programming language and a (toy) theorem prover. As of August 2019, it is no more
than a skeleton. Currently, there is an absolutely minimal dependently typed core
lambda calculus with lambda abstractions, Pi types, `let` constructs and a single
type universe `Set : Set`.

## Motivation

Since secondary school, I have been amazed by the enormous power and magic-like
capabilities of theorem provers and dependent types, in particular the "big trinity"
of CC-rooted languages, [Coq/Gallina](https://coq.inria.fr/),
[Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php) and
[Idris](https://www.idris-lang.org/).

For some time, I lived a happy, complacent certified life. Then, suddenly, in short
order, [F*](https://www.fstar-lang.org/) and [Lean](https://leanprover.github.io/)
appeared. These two fellas changed my view of theorem provers for years to come. They
showed me that their potential is much greater than I perceived and that their
usability can be improved beyond what I thought possible. The user experience aspect
of their innovative approach appealed to me especially.

Being enchanted by Haskell at the same time, I set out to create a toy, yet
non-trivial prover of myself.

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

## Interactive prompt

The `simurgh-exe` target provides a simple interactive prompt (or REPL) to test the
features of the language. If a valid term is entered on the command line, it is
evaluated and the result is printed out (no type checking of the input yet).

Currently, the following commands are supported:

-   `:help` will print the help message which currently lists the available commands,
    i.e. similarly to this very list.
-   `:quit` will terminate the REPL.
-   `:type <expr>` will infer and print the type of the given expression.

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

## Roadmap/Wishlist

_(Hint: Click the triangle ⯈ to expand the individual subsections.)_

<details>
<summary>Regarding the language, listed roughly in ascending order of difficulty,
interestingness and distance in time:</summary>

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
</details>

<details>
<summary>Regarding infrastructure and libraries:</summary>

- [ ] Get Haskeline to [support Text](https://github.com/judah/haskeline/issues/80)
- [ ] Consider an alternative Prelude, such as
    [Protolude](https://www.stackage.org/lts-14.2/package/protolude)
- [ ] Thorough tests, at least for the parser and type checker
- [ ] Introduce property tests -- QuickCheck or
    [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog)
- [ ] Continuous integration with Travis
- [ ] Create a couple of simple examples of the language
- [ ] Create a prelude and a hint of a standard library
- [ ] Formalize an easy, classical theorem, e.g. the infinitude of natural numbers,
    irrationality of √2, infinitude of primes…
- [ ] Modern pretty-printing – [Hughes/PJ](https://hackage.haskell.org/package/pretty),
    [Wadler/Leijen](https://hackage.haskell.org/package/ansi-wl-pprint),
    [mainland-pretty](https://hackage.haskell.org/package/mainland-pretty) or
    [prettyprinter](https://github.com/quchen/prettyprinter)
- [ ] Improve parsing and error reporting; look into
    [Trifecta](https://hackage.haskell.org/package/trifecta)
- [ ] See if lenses can be leveraged anywhere
</details>

## References

Reference papers and books, learning resources, study materials, etc. will be
listed here in no particular order. Currently, the list consists partly of articles
that inspired me the most and partly of papers illustrating the direction where I'd
like to be headed.

-   Löh, A\., McBride, C. & Swierstra, W. (2007). [Simply Easy! An Implementation of a Dependently Typed Lambda Calculus](http://strictlypositive.org/Easy.pdf).
-   Altenkirch, T\. _et al._ (2010). [ΠΣ: Dependent Types without the Sugar](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf).
-   Abel, A\. & Altenkirch, T. (2008). [A Partial Type Checking Algorithm for Type : Type](http://www.cs.nott.ac.uk/~psztxa/publ/msfp08.pdf).
-   Altenkirch, T\. & McBride, C. (2006). [Towards Observational Type Theory](http://strictlypositive.org/ott.pdf).
-   Altenkirch, T\., McBride, C. & Swierstra, W. (2007). [Observational Equality, Now!](http://www.cs.nott.ac.uk/~psztxa/publ/obseqnow.pdf).
-   de Moura, L\. et al\. (2015). [Elaboration in Dependent Type Theory](https://arxiv.org/abs/1505.04324). _Unpublished._
-   de Moura, L\. et al\. (2015). [The Lean Theorem Prover (system description)](https://leanprover.github.io/papers/system.pdf).
-   Gratzer, D\. (2014). [Bidirectional Type Checkers for λ→ and λΠ](https://jozefg.bitbucket.io/posts/2014-11-22-bidir.html).
-   Paulin-Mohring, C\. (2014). [Introduction to the Calculus of Inductive Constructions](https://hal.inria.fr/hal-01094195/).
-   Eisenberg, R\. A. (2018). [Stitch: The Sound Type-Indexed Type Checker](https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.pdf).

Also inspired by

-   Stephanie Weirich's [pi-forall](https://github.com/sweirich/pi-forall)
-   Mörtberg et al.'s [implementation of Cubical Type Theory](https://github.com/mortberg/cubicaltt)
-   Bauer at al.'s [Andromeda](https://andromedans.github.io/andromeda/)

Back in 2015, I started collecting and sorting resources on the topic of formal
methods in general. Though I haven't been as keen on updating it since, some of the
results are available in my [fm-notes repository](https://mgrabovsky.net/fm-notes/).

## Licence

The code, documentation and other resources in this repository are provided under the
conditions of the [Blue Oak Model License 1.0.0](https://blueoakcouncil.org/license/1.0.0).

