module Simurgh.Eval
    ( eval
    ) where

import Control.Applicative       ((<|>))
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import Unbound.Generics.LocallyNameless

import Simurgh.Syntax

-- Evaluator for our toy untyped lambda calculus.
-- Based on a small-step, call-by-value operational semantics.

done :: MonadPlus m => m a
done = mzero

-- | Try to perform a single step of evaluation
step :: Expr -> MaybeT FreshM Expr
step (App (Lam b) args) = do
    -- Unbind the binder into the telescope and the body where it's bound.
    (telescope, body) <- unbind b
    -- 'vars' is the list of names bound by the lambda. It is the "domain" 
    -- of the associated telescope.
    let vars  = domain telescope
        nvars = length vars
        nargs = length args
    -- Quite a complicated code in order to support partial application.
    if nvars < nargs
       then let argsTaken = take nvars args
                newBody   = substs (zip vars argsTaken) body
             in pure $ App newBody (drop nvars args)
       else if nvars == nargs
       then -- Simultaneously substitute arguments in place of their corresponding bound
            -- variables inside the body of the lambda.
            pure $ substs (zip vars args) body
       else let varsTaken     = take nargs vars
                teleRemaining = dropTele nargs telescope
                newLambda     = Lam (bind teleRemaining body)
             in pure $ substs (zip varsTaken args) newLambda
step (App t1 args) =
    -- We have an application of something other than a lambda. Try to first reduce
    -- then left-hand term and then the right-hand terms.
    (App <$> step t1 <*> pure args) <|>
    -- The following will not reduce in steps, but it will reduce the arguments
    -- at once.
    (App t1 <$> traverse step args)
step (Let b) = do
    ((x, Embed t), body) <- unbind b
    pure $ subst x t body
step _ = done

transitiveClosure :: Monad m => (a -> MaybeT m a) -> (a -> m a)
transitiveClosure f a = do
    ma' <- runMaybeT (f a)
    case ma' of
      Just a' -> transitiveClosure f a'
      Nothing -> pure a

-- | Evaluate (normalize) a term of our toy untyped lambda calculus.
-- This is a transitive closure of 'step'.
eval :: Expr -> Expr
eval = runFreshM . transitiveClosure step

-- TODO: Implement reduction to weak head normal form (WHNF).

-- More arguments than binders.
ex1 = App (mkLam [("xxx", Set0), ("yyy", Set0), ("fff", mkPi [("_", mkVar "xxx")] (mkVar "yyy")), ("aaa", mkVar "xxx")] (App (mkVar "fff") [mkVar "aaa"]))
          [mkVar "Nat", mkVar "Unit", mkLam [("_", mkVar "Nat")] (mkVar "tt"), mkVar "3", mkVar "extra", Set0]

-- Assertion for this test.
as1 = eval ex1 `aeq` App (mkVar "tt") [mkVar "extra", Set0]

-- As many binders as arguments.
ex2 = App (mkLam [("xxx", Set0), ("yyy", Set0), ("fff", mkPi [("_", mkVar "xxx")] (mkVar "yyy")), ("aaa", mkVar "xxx")] (App (mkVar "fff") [mkVar "aaa"]))
          [mkVar "Nat", mkVar "Unit", mkLam [("_", mkVar "Nat")] (mkVar "tt"), mkVar "3"]

-- Assertion for this test.
as2 = eval ex2 `aeq` mkVar "tt"

-- More binders than arguments.
ex3 = App (mkLam [("xxx", Set0), ("yyy", Set0), ("fff", mkPi [("_", mkVar "xxx")] (mkVar "yyy")), ("aaa", mkVar "xxx")] (App (mkVar "fff") [mkVar "aaa"]))
          [mkVar "Nat", mkVar "Unit"]

-- Assertion for this test.
as3 = eval ex3 `aeq` mkLam [("f", mkPi [("_", mkVar "Nat")] (mkVar "Unit")), ("a", mkVar "Nat")] (App (mkVar "f") [mkVar "a"])

