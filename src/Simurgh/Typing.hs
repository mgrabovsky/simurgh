{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simurgh.Typing
    ( runTyping
    ) where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Typeable              (Typeable)
import GHC.Generics               (Generic)

import Unbound.Generics.LocallyNameless

import Simurgh.Eval
import Simurgh.Syntax

-- | Convertibility of λ terms.
(~=) :: Expr -> Expr -> FreshM Bool
e1 ~= e2
  | e1 `aeq` e2 = pure True
  | otherwise   = do
      -- Originally, this was full reduction to the normal form rather than whnf.
      e1' <- whnf e1
      e2' <- whnf e2
      if e1' `aeq` e1 && e2' `aeq` e2
         then pure False
         else e1 ~= e2

type TypingM = ExceptT String LFreshM

data Telescope = Empty
               | Cons (Rebind (Name Expr, Embed Expr) Telescope)
               deriving (Generic, Show, Typeable)

instance Semigroup Telescope where
  Empty                       <> t2 = t2
  Cons (unrebind -> (p, t1')) <> t2 = Cons (rebind p (t1' <> t2))

instance Monoid Telescope where
  mempty = Empty

instance Alpha Telescope
instance Subst Expr Telescope

single :: (Name Expr, Embed Expr) -> Telescope
single xty = Cons (rebind xty Empty)

lookUp :: Name Expr -> Telescope -> TypingM Expr
lookUp n Empty = throwE $ "Not in scope: " <> show n
lookUp v (Cons (unrebind -> ((x, Embed a), t')))
  | v == x    = pure a
  | otherwise = lookUp v t'

-- Type checker -- bidirectional (uni- for now) type inference.

unPi :: Expr -> TypingM (Bind (Name Expr, Embed Expr) Expr)
unPi (Pi b) = pure b
unPi t      = throwE $ "Unexpected Pi type: " <> show t

infer :: Telescope -> Expr -> TypingM Expr
infer g (Var x) = lookUp x g
infer _ Set0    = pure Set0
infer g (Lam b) = lunbind b $ \(xty, body) -> do
    bodyType <- infer (g <> single xty) body
    pure $ Pi (bind xty bodyType)
infer g (App applicand argument) = do
    b <- unPi =<< infer g applicand
    lunbind b $ \((x, Embed ty), bty) -> do
        check g argument ty
        pure $ subst x argument bty
infer g (Pi b) = lunbind b $ \(xty, body) -> do
    check (g <> single xty) body Set0
    pure Set0
infer g (Let b) = lunbind b $ \((x, Embed t), body) -> do
    xty <- infer g t
    infer (g <> single (x, embed xty)) body

check :: Telescope -> Expr -> Expr -> TypingM ()
check g m a = do
    b <- infer g m
    checkEq b a

-- | A very limited notion of equality of types.
-- TODO: Consider convertibility (~=) instead.
checkEq :: Expr -> Expr -> TypingM ()
checkEq t1 t2 = if aeq t1 t2
                   then pure ()
                   else throwE $ "Could not match " <> show t1 <>
                       " with " <> show t2

runTyping :: Expr -> Either String Expr
runTyping = runLFreshM . runExceptT . infer Empty

