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
      -- Originally, this was full reduction to the normal form rather than whnf.
      e1' <- whnf e1
      e2' <- whnf e2
      if e1' `aeq` e1 && e2' `aeq` e2
         then pure False
         else e1 ~= e2

type TypingM = ExceptT String LFreshM

lookUp :: Name Expr -> Telescope -> TypingM Expr
lookUp n Empty = throwE $ "Not in scope: " <> show n
lookUp v (Cons (unrebind -> ((x, Embed a), t')))
  | v == x    = pure a
  | otherwise = lookUp v t'

-- Type checker -- bidirectional (uni- for now) type inference.

unPi :: Expr -> TypingM (Bind Telescope Expr)
unPi (Pi b) = pure b
unPi t      = throwE $ "Expected a Pi type, but got " <> show t

infer :: Telescope -> Expr -> TypingM Expr
infer g (Var x) = lookUp x g
infer _ Set0    = pure Set0
infer g (Lam b) = lunbind b $ \(delta, m) -> do
    assertTypes g delta
    bty <- infer (g <> delta) m
    pure (Pi (bind delta bty))
infer g (App left rights) = do
    b <- unPi =<< infer g left
    lunbind b $ \(delta, bty) -> do
        checkList g rights delta
        multiSubst delta rights bty
infer g (Pi b) = lunbind b $ \(delta, bty) -> do
    assertTypes g delta
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

-- | @'assertTypes' delta tele@ checks that all the variables bound in the telescope
-- @tele@ bind to well-formed types in the context @delta@.
assertTypes :: Telescope -> Telescope -> TypingM ()
assertTypes _     Empty          = pure ()
assertTypes delta (Cons binders) = do
    let ((x, Embed ty), binders') = unrebind binders
    check delta ty Set0
    let delta' = Cons $ rebind (x, Embed ty) delta
    assertTypes delta' binders'

multiSubst :: Telescope -> [Expr] -> Expr -> TypingM Expr
multiSubst  Empty    []      t = pure t
multiSubst (Cons rb) (t1:ts) t = multiSubst t1' ts t'
    where ((x, _), t1') = unrebind rb
          t'            = subst x t1 t

-- | @'check' delta e t@ checks that the term @e@ has type @t@ in the context
-- @delta@.
check :: Telescope -> Expr -> Expr -> TypingM ()
check g m a = do
    b <- infer g m
    checkEq b a

-- | A very limited notion of equality.
-- TODO: Consider convertibility (~=).
checkEq :: Expr -> Expr -> TypingM ()
checkEq t1 t2 = if aeq t1 t2
                   then pure ()
                   else throwE $ "Expected type " <> show t1 <>
                       ", but got " <> show t2

runTyping :: Expr -> Either String Expr
runTyping = runLFreshM . runExceptT . infer Empty

