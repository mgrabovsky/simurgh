This might be a moderately simple dependently typed thing one day. It is no more than
a skeleton as of January 2019.

## Roadmap/Wishlist

- [x] Modern library for binding syntax – [Unbound](https://hackage.haskell.org/package/unbound-generics)
- [ ] Modern pretty-printing – [Hughes/PJ](https://hackage.haskell.org/package/pretty),
  [Wadler/Leijen](https://hackage.haskell.org/package/ansi-wl-pprint),
  or [mainland-pretty](https://hackage.haskell.org/package/mainland-pretty)
- [ ] Tests
- [ ] Improve parsing and error reporting. Look into
  [Trifecta](https://hackage.haskell.org/package/trifecta)
- [ ] See if lenses can be used anywhere

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

