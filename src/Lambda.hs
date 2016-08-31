{-# LANGUAGE RecordWildCards #-}

module Lambda ( Term(..)
              , Type
              , Id
              , open
              , varOpen
              , varClose
              , subst
              , isLocallyClosed
              , runTyping
              , inferType
              , preludeEnv
              ) where

import Control.Monad (unless)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Function (on)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Numeric.Natural

-- --------------------------------------------------------
-- * Syntax
-- --------------------------------------------------------

type Id = String
data Term = BVar Natural
          | FVar Id
          | Lam  Type Term
          | App  Term Term

          | Pi   Type Term
          | TySet
          | TyEmpty
          | TyUnit
          | TyBool

          | TmUnit
          | TmTrue
          | TmFalse

          -- Eliminators for primitive types
          | Ite     Term Type Term Term
          | IteL    Term Type Type
          | ExFalso Term Type
       -- | UnitElim
          deriving (Eq, Ord)

type Type = Term

instance Show Term where
        show (BVar n)    = '^' : show n
        show (FVar x)    = x
        show (Lam ty t)  = "\x1b[33mλ\x1b[0m[" ++ show ty ++ "]. " ++ show t
        show (App t1 t2) = wrap' t1 ++ " " ++ wrap t2
        show (Pi ty t)   = if isLocallyClosed t
                               then wrap' ty ++ " \x1b[33m→\x1b[0m " ++ show t
                               else "\x1b[33mΠ\x1b[0m[" ++ show ty ++ "]. " ++ show t
        show TySet       = "Set"
        show TyEmpty     = "0"
        show TyUnit      = "1"
        show TyBool      = "2"
        show TmUnit      = "tt"
        show TmTrue      = "T"
        show TmFalse     = "F"
        show (Ite b ty t f) = "\x1b[35mif\x1b[0m " ++ wrap b ++ " / " ++ wrap ty ++
                                   " \x1b[35mthen\x1b[0m " ++ wrap t ++
                                   " \x1b[35melse\x1b[0m " ++ wrap f
        show (IteL b t f) = "\x1b[35mIf\x1b[0m " ++ wrap b ++
                                   " \x1b[35mThen\x1b[0m " ++ wrap t ++
                                   " \x1b[35mElse\x1b[0m " ++ wrap f
        show (ExFalso e ty) = "\x1b[35mexfalso\x1b[0m " ++ wrap e ++ " \x1b[35m!\x1b[0m " ++ wrap ty

wrap' (App t1 t2) = wrap' t1 ++ " " ++ wrap t2
wrap' t           = wrap t

wrap t | isNaked t = show t
       | otherwise = "(" ++ show t ++ ")"
       where
            isNaked (Lam {})     = False
            isNaked (App {})     = False
            isNaked (Pi {})      = False
            isNaked (Ite {})     = False
            isNaked (IteL {})    = False
            isNaked (ExFalso {}) = False
            isNaked _            = True

-- | Open a term by substituting another term for the top bound variable
open u t = go 0 u t
    where
        go k u t@(BVar m)
            | m > k     = BVar (pred m)
            | m == k    = u
            | otherwise = t
        go k u (Lam ty t)     = Lam (go k u ty) (go (succ k) u t)
        go k u (App t1 t2)    = App (go k u t1) (go k u t2)
        go k u (Pi ty t)      = Pi (go k u ty) (go (succ k) u t)
        go k u (Ite b ty t f) = Ite (go k u b) (go (succ k) u ty) (go k u t) (go k u f)
        go k u (IteL b t f)   = IteL (go k u b) (go k u t) (go k u f)
        go k u (ExFalso e ty) = ExFalso (go k u e) (go k u ty)
        go _ _ t              = t

-- | Open a term with respect to the top bound variable
varOpen x = open (FVar x)

-- | Close a term with respect to a free variable
varClose x t = go 0 x t
    where
        go k x t@(FVar y)  = if y == x then BVar k else t
        go k x (Lam ty t)  = Lam (go k x ty) (go (succ k) x t)
        go k x (App t1 t2) = App (go k x t1) (go k x t2)
        go k x (Pi ty t)   = Pi (go k x ty) (go (succ k) x t)
        go k x (Ite b ty t f) = Ite (go k x b) (go (succ k) x ty) (go k x t) (go k x f)
        go k x (IteL b t f) = IteL (go k x b) (go k x t) (go k x f)
        go k x (ExFalso e ty) = ExFalso (go k x e) (go k x ty)
        go _ _ t           = t

-- | Substitute a term for a free variable
subst x u = open u . varClose x

-- | Check whether a term is locally closed, i.e. all bound variables
-- have their corresponding binding abstractions
isLocallyClosed t = go 0 t
    where
        -- FIXME: Repair after figuring out the dependencies in Ite and IteL
        go k (BVar m)       = m < k
        go k (Lam ty t)     = go k ty && go (succ k) t
        go k (App t1 t2)    = go k t1 && go k t2
        go k (Pi ty t)      = go k ty && go (succ k) t
        go k (Ite b ty t f) = and (go k <$> [b, ty, t, f])
        go k (IteL b t f)   = and (go k <$> [b, t, f])
        go k (ExFalso e ty) = go k e && go k ty
        go _ _              = True

-- --------------------------------------------------------
-- * Evaluation, reduction and conversion
-- --------------------------------------------------------

{-
isNeutral (BVar _)  = True
isNeutral (FVar _)  = True
isNeutral (App t _) = isNeutral t
isNeutral _         = False
-}

-- TODO
whnf t = t :: Term

eval (Lam ty t) = Lam (eval ty) (eval t)
eval (App t u)  = app (eval t) (eval u)
    where
        app (Lam _ t) u = eval (open u t)
        app t         u = App t u
eval (Pi ty t)  = Pi (eval ty) (eval t)
eval (Ite b _ t f) = case eval b of
                         TmTrue  -> eval t
                         TmFalse -> eval f
                         x -> IteL x (eval t) (eval f) -- FIXME?
eval (IteL b t f) = case eval b of
                        TmTrue  -> eval t
                        TmFalse -> eval f
                        x -> IteL x (eval t) (eval f) -- FIXME?
eval (ExFalso e t) = ExFalso (eval e) (eval t)
eval t = t

conv = (==) `on` eval

-- --------------------------------------------------------
-- * Typing
-- --------------------------------------------------------

data TEnv = TEnv
          { boundVars :: [Type]
          , freeVars  :: Map Id Type
          } deriving (Eq, Ord)

instance Show TEnv where
        show (TEnv {..}) =
            let bvars = zipWith (\n ty -> "  "  ++ show n ++ ": " ++ show ty)
                            [0..] boundVars
                fvars = M.foldrWithKey' (\x ty -> (:) $ "  " ++ x ++ ": " ++ show ty)
                            [] freeVars
             in "bound:\n" ++ (unlines bvars) ++ "\nfree:\n" ++ (unlines fvars)

instance Monoid TEnv where
        mempty = TEnv [] M.empty
        mappend (TEnv b1 f1) (TEnv b2 f2) = TEnv (b1 ++ b2) (f1 `M.union` f2)

type Typing m = ExceptT String (ReaderT TEnv m)

boundType :: Monad m => Natural -> Typing m Type
boundType n = do
        tys <- drop (fromEnum n) <$> asks boundVars
        case tys of
            ty:_ -> pure ty
            _    -> throwError "Bound variable doesn't have its corresponding binder"

pushType :: Type -> TEnv -> TEnv
pushType ty env = env { boundVars = ty : boundVars env }

freeType :: Monad m => Id -> Typing m Type
freeType x = do
        tys <- asks freeVars
        case M.lookup x tys of
            Just ty -> pure ty
            _       -> throwError ("Free variable '" ++ x ++ "' doesn't have a corresponding type")

-- Bidirectional type checking

infer :: Monad m => Term -> Typing m Type
infer (BVar n) = shiftIndices (succ n) . eval <$> boundType n
infer (FVar x) = eval <$> freeType x
infer (Lam ty t) = do
        check ty TySet
        tty <- eval <$> local (pushType ty) (infer t)
        pure (Pi ty tty)
infer (App t1 t2) = do
        ty1 <- infer t1
        case ty1 of
            Pi bty tty -> check t2 bty $> eval (open t2 tty)
            _          -> throwError "Invalid application"
infer (Pi ty t) = check ty TySet *> local (pushType ty) (check t TySet) $> TySet
infer TySet = pure TySet
infer TyEmpty = pure TySet
infer TyUnit = pure TySet
infer TyBool = pure TySet
infer TmUnit = pure TyUnit
infer TmTrue = pure TyBool
infer TmFalse = pure TyBool
-- FIXME: Incorrect. How to work with the @b@ dependency inside branches?
infer (Ite b ty t f) = do
        check b TyBool
        local (pushType TyBool) (check ty TySet)
        check t (open TmTrue ty)
        check f (open TmFalse ty)
        pure (eval $ open b ty)
infer (IteL b t f) = do
        check b TyBool
        local (pushType TyBool) (check t TySet *> check f TySet)
        pure TySet
infer (ExFalso e ty) = check e TyEmpty *> check ty TySet $> ty

check :: Monad m => Term -> Type -> Typing m ()
check t ty = do
        ty' <- infer t
        unless (conv ty ty') (throwError $ "Invalid type of '" ++ show t ++
               "':\n  expected '\x1b[1;32m" ++ show ty ++ "\x1b[0m'\n       got '\x1b[1;31m" ++
               show ty' ++ "\x1b[0m'")
                                --
-- FIXME: WTF? Can we get rid of this ugly hack?
shiftIndices m (BVar n)       = BVar (m + n)
shiftIndices m (Pi ty t)      = Pi (shiftIndices m ty) (shiftIndices (succ m) t)
shiftIndices m (Lam ty t)     = Lam (shiftIndices m ty) (shiftIndices (succ m) t)
shiftIndices m (App t1 t2)    = App (shiftIndices m t1) (shiftIndices m t2)
shiftIndices m (Ite b ty t f) = Ite (shiftIndices m b) (shiftIndices m ty)
                                    (shiftIndices m t) (shiftIndices m f)
shiftIndices m (IteL b t f)   = IteL (shiftIndices m b) (shiftIndices m t)
                                    (shiftIndices m f)
shiftIndices m (ExFalso e ty) = ExFalso (shiftIndices m e) (shiftIndices m ty)
shiftIndices _ t              = t

runTyping t = runReaderT (runExceptT (infer t))
inferType t = runIdentity (runTyping t preludeEnv)

eqEnv = TEnv [] (M.fromList types)
    where types = [ ("eq",   Pi TySet (Pi (BVar 0) (Pi (BVar 1) TySet)))
                  , ("refl", Pi TySet (Pi (BVar 0) (App (App (App (FVar "eq") (BVar 1)) (BVar 0)) (BVar 0))))
                  , ("eq_ind", Pi TySet
                               (Pi (BVar 0)
                               (Pi (Pi (BVar 1) TySet)
                               (Pi (App (BVar 0) (BVar 1))
                               (Pi (BVar 3)
                               (Pi (App (App (App (FVar "eq") (BVar 4)) (BVar 3)) (BVar 0))
                               (App (BVar 3) (BVar 1))))))))
                  ]

natEnv = TEnv [] (M.fromList types)
    where types = [ ("nat", TySet)
                  , ("Z",   FVar "nat")
                  , ("S",   Pi (FVar "nat") (FVar "nat"))
                  , ("nat_ind", Pi (Pi (FVar "nat") TySet)
                                (Pi (App (BVar 0) (FVar "Z"))
                                (Pi (Pi (FVar "nat")
                                    (Pi (App (BVar 2) (BVar 0))
                                    (App (BVar 3) (App (FVar "S") (BVar 1)))))
                                (Pi (FVar "nat")
                                (App (BVar 3) (BVar 0))))))
                  ]

listEnv = TEnv [] (M.fromList types)
    where types = [ ("list", Pi TySet TySet)
                  , ("nil",  Pi TySet (App (FVar "list") (BVar 0)))
                  , ("cons", Pi TySet (Pi (BVar 0) (Pi (App (FVar "list") (BVar 1))
                                (App (FVar "list") (BVar 2)))))
                  ]

vectEnv = TEnv [] (M.fromList types)
    where types = [ ("vector", Pi TySet (Pi (FVar "nat") TySet))
                  , ("vnil"  , Pi TySet (FVar "vector" @@ BVar 0 @@ FVar "Z"))
                  , ("vcons",  Pi TySet (Pi (FVar "nat") (Pi (BVar 1)
                                    (Pi (FVar "vector" @@ BVar 2 @@ BVar 1)
                                        (FVar "vector" @@ BVar 3 @@ (FVar "S" @@ BVar 2))))))
                  ]
          (@@) = App

logicEnv = TEnv [] (M.fromList types)
    where types = [ ("and", Pi TySet (Pi TySet TySet))
                  , ("conj", Pi TySet (Pi TySet (Pi (BVar 1) (Pi (BVar 1) (App (App (FVar "and") (BVar 3)) (BVar 2))))))
                  , ("or", Pi TySet (Pi TySet TySet))
                  , ("orl", Pi TySet (Pi TySet (Pi (BVar 1) (App (App (FVar "or") (BVar 2)) (BVar 1)))))
                  , ("orr", Pi TySet (Pi TySet (Pi (BVar 0) (App (App (FVar "or") (BVar 2)) (BVar 1)))))
                  ]

preludeEnv = eqEnv <> natEnv <> listEnv <> vectEnv <> logicEnv

shortList = App (App (App (FVar "cons") TyBool) TmFalse)
                (App (App (App (FVar "cons") TyBool) TmTrue)
                     (App (FVar "nil") TyBool))

shortProof = let f = FVar
                 app = foldl1 App
              in app [f "conj", app [f "eq", f "nat", f "Z", f "Z"],
                                app [f "eq", f "nat", app [f "S", f "Z"], app [f "S", f "Z"]],
                                app [f "refl", f "nat", f "Z"],
                                app [f "refl", f "nat", app [f "S", f "Z"]]]

shortProof' = let f = FVar
                  app = foldl1 App
               in app [f "eq_ind", TyBool, TmTrue,
                      Lam TyBool (IteL (BVar 0) TyUnit TyEmpty), TmUnit, TmFalse]

-- Proof of transitivity of equality, doesn't typecheck:
-- > fun A : Set => fun a : A => fun b : A => fun c : A => fun Hab : eq A a b => fun Hbc : eq A b c => eq_ind A b (fun x:A => eq A a x) Hab c Hbc

