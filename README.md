This might be a moderately simple dependently typed thing one day. It is no more than
a skeleton as of January 2019. Currently, there is an absolutely minimal core
dependently typed lambda calculus with lambda terms, Pi types and a single type `Set`
of type `Set`.

## Roadmap/Wishlist

- [x] Modern library for binding syntax – [Unbound](https://hackage.haskell.org/package/unbound-generics)
- [ ] Modern pretty-printing – [Hughes/PJ](https://hackage.haskell.org/package/pretty),
  [Wadler/Leijen](https://hackage.haskell.org/package/ansi-wl-pprint),
  or [mainland-pretty](https://hackage.haskell.org/package/mainland-pretty)
- [ ] Tests
- [ ] Improve parsing and error reporting. Look into
  [Trifecta](https://hackage.haskell.org/package/trifecta)
- [ ] See if lenses can be leveraged anywhere
- [ ] Pattern matching
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

Also inspired by

-   Stephanie Weirich's [pi-forall](https://github.com/sweirich/pi-forall)
-   Mörtberg et al.'s [implementation of Cubical Type Theory](https://github.com/mortberg/cubicaltt)
-   Bauer at al.'s [Andromeda](https://andromedans.github.io/andromeda/)

