In Simurgh, we're using the Unbound library for binding syntax -- that is binding
variables inside lambda abstractions, `let-in` constructs, etc. Specifically, the
[unbound-generics](https://hackage.haskell.org/package/unbound-generics)
implementation of the original Weirich, Yorgey, Sheard (2011) [Binders
Unbound](https://www.seas.upenn.edu/~sweirich/papers/icfp11.pdf).

This document lists and briefly describes the basic data types and functions that the
library provides. Although it might be more or less futile, since the original
article is written so well. Go read it!

## Patterns and terms

There are two syntactic categories we need to discern: pattern types `p₁, p₂, ...`
and term types `t₁, t₂, ...`. Patterns specify which variables are to be bound within
terms.

*   `Bind p t` is a term in which names in `p` are bound within the term `t`.
*   `Embed t` allows to embed terms within patterns. If `t` is a term, `Embed t` is
    a pattern, but names are not bound along with the rest of the pattern.
*   `Rebind p₁ p₂` allows for nested binding. If `p₁` and `p₂` are patterns, so is
    `Rebind p₁ p₂` and terms embedded in `p₂` may refer to binders in `p₁`.
*   `Rec p` allows for recursive binding. If `p` is a pattern, `Rec p` is a recursive
    pattern such that `p` may bind names in terms embedded within itself.
*   `TRec p` is a standalone version of `Rec`; `TRec p` is a term type. It is
    identical to `Bind (Rec p) ()`.
*   `Shift p` is a pattern. It shifts the scope of an embedded term one level
    outwards.

