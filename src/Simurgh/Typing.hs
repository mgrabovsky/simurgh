module Simurgh.Typing
    ( runTyping
    ) where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import Unbound.Generics.LocallyNameless

import Simurgh.Eval
import Simurgh.Syntax

-- | Convertibility of λ terms.
(~=) :: Expr -> Expr -> FreshM Bool
e1 ~= e2
  | e1 `aeq` e2 = pure True
  | otherwise   = do
      e1' <- red e1
      e2' <- red e2
      if e1' `aeq` e1 && e2' `aeq` e2
         then pure False
         else e1 ~= e2

-- TODO: Move all evaluation-related functionality into one module.
-- TODO: Implement CoC conversion rules.
red :: Expr -> FreshM Expr
red (App e1 args) = do
    e1'   <- red e1
    args' <- traverse red args
    case e1' of
        Lam b -> do
            (domain -> vars, body) <- unbind b
            pure $ substs (zip vars args') body
        _ -> pure $ App e1' args'
red (Lam b) = do
    (tele, e) <- unbind b
    e' <- red e
    pure $ Lam (bind tele e')
red (Let b) = do
    ((x, unembed -> t), body) <- unbind b
    red $ subst x t body
red e = pure e

type TypingM = ExceptT String LFreshM

lookUp :: Name Expr -> Telescope -> TypingM Expr
lookUp n Empty = throwE $ "Not in scope: " <> show n
lookUp v (Cons (unrebind -> ((x, Embed a), t')))
  | v == x    = pure a
  | otherwise = lookUp v t'

-- Type checker -- bidirectional (uni- for now) type inference.

unPi :: Expr -> TypingM (Bind Telescope Expr)
unPi (Pi b) = pure b
unPi t      = throwE $ "Unexpected Pi type: " <> show t

infer :: Telescope -> Expr -> TypingM Expr
infer g (Var x) = lookUp x g
infer _ Set0    = pure Set0
infer g (Lam b) = lunbind b $ \(delta, m) -> do
    bty <- infer (g <> delta) m
    pure (Pi (bind delta bty))
infer g (App left rights) = do
    b <- unPi =<< infer g left
    lunbind b $ \(delta, bty) -> do
        checkList g rights delta
        multiSubst delta rights bty
infer g (Pi b) = lunbind b $ \(delta, bty) -> do
    check (g <> delta) bty Set0
    pure Set0
infer g (Let b) = lunbind b $ \((x, Embed t), body) -> do
    xty <- infer g t
    infer (Cons (rebind (x, embed xty) g)) body

checkList :: Telescope -> [Expr] -> Telescope -> TypingM ()
checkList _ []        Empty    = pure ()
checkList g (t:rest) (Cons rb) = do
    let ((x, Embed a), t') = unrebind rb
    check g t a
    checkList (subst x t g) (subst x t rest) (subst x t t')
checkList _ _         _        = throwE "Unequal number of arguments and parameters"

multiSubst :: Telescope -> [Expr] -> Expr -> TypingM Expr
multiSubst  Empty    []      t = pure t
multiSubst (Cons rb) (t1:ts) t = multiSubst t1' ts t'
    where ((x, _), t1') = unrebind rb
          t'            = subst x t1 t

check :: Telescope -> Expr -> Expr -> TypingM ()
check g m a = do
    b <- infer g m
    checkEq b a

-- | A very limited notion of equality.
checkEq :: Expr -> Expr -> TypingM ()
checkEq t1 t2 = if aeq t1 t2
                   then pure ()
                   else throwE $ "Could not match " <> show t1 <>
                       " with " <> show t2

runTyping :: Expr -> Either String Expr
runTyping = runLFreshM . runExceptT . infer Empty

{- Environment with equality:
    eq     : forall (A : Set) (x y : A), Set
    refl   : forall (A : Set) (x : A), eq A x x
    eq_ind : forall (A : Set) (x : A) (T : A -> Set)
                    (B : T x) (y : A),
                    eq A x y -> T y

Environment with the empty type, the unit type and booleans:
    empty : Set
    -- TODO: empty_ind, unit_ind, bool_ind
    unit  : Set
    tt    : unit
    bool  : Set
    true  : bool
    false : bool

Environment with natural numbers:
    nat     : Set
    Z       : nat
    S       : nat -> nat
    nat_ind : forall (T : nat -> Set) (TZ : T Z)
                     (TS : forall (n : nat), T n -> T (S n))
                     (n : nat), T n

Environment with lists:
    list : Set -> Set
    nil  : forall (A : Set), list A
    cons : forall (A : Set) (head : A) (tail : list A), list A

Environment with vectors:
    vect  : Set -> nat -> Set
    vnil  : forall (A : Set), vect A Z
    vcons : forall (A : Set) (n : nat) (head : A) (tail : vect A n),
            vect A (S n)

Environment with logical connectives:
    and  : Set -> Set -> Set
    conj : forall (P : Set) (Q : Set), P -> Q -> and P Q
    or   : Set -> Set -> Set
    orl  : forall (P : Set) (Q : Set), P -> or P Q
    orr  : forall (P : Set) (Q : Set), Q -> or P Q

Short proofs:
    -- 0 = 0 /\ 1 = 1
    conj (eq nat Z Z) (eq nat (S Z) (S Z)) (refl nat Z) (refl nat (S Z))
    -- true /= false
    -- TODO: if_then_else |--> bool_ind
    eq_ind bool true
        (fun (b : bool) => if b then unit else empty)
        tt false -- : eq bool true false -> empty

Proof of transitivity of equality:
    fun (A : Set) (a : A) (b : A) (c : A)
        (Hab : eq A a b) (Hbc : eq A b c)
     => eq_ind A b (fun (x : A) => eq A a x) Hab c Hbc

Mildly interesting term for the type checker:
    fun (A : Set) (B : A -> Set) (x : A) (t : A -> B x) => t x
Currently infers the expected type:
    Π (A : Set) (B : Π (_ : A) => Set) (x : A) (t : Π (_ : A) => (B x)) => (B x)
-}

