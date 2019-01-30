module Simurgh.Eval
    ( eval
    , whnf
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
step (App (Lam b) argument) = do
    -- Unbind the binder into the variable+type pair and the body where it's bound.
    ((x, _), body) <- unbind b
    -- Reduce the applicand by substituting first argument into the body.
    pure $ subst x argument body
step (App applicand argument) =
    -- We have an application of something other than a lambda. Try to first reduce
    -- then left-hand term and then the right-hand terms.
    (App <$> step applicand <*> pure argument) <|>
    -- The following will not reduce in steps, but it will reduce the arguments
    -- at once.
    (App applicand <$> step argument)
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

-- TODO: Implement and comment CoC conversion rules.
-- TODO: Check this algorithm for correctness.
-- TODO: Consider LFresh in place of Fresh. Possibly even a pure interface.
whnf :: Fresh m => Expr -> m Expr
whnf (App applicand argument) = do
    -- First reduce to applicand.
    nf1 <- whnf applicand
    case nf1 of
        -- If the applicand reduces to a lambda, unbind it and substitute the
        -- argument.
        Lam b -> do
            ((x, _), body) <- unbind b
            whnf $ subst x argument body
        -- Otherwise only keep the applicand reduced.
        _ -> pure $ App nf1 argument
whnf (Let b) = do
    ((x, Embed rhs), body) <- unbind b
    whnf $ subst x rhs body
-- TODO: We might want to unfold definitions once we implement contexts.
whnf t = pure t

