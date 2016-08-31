{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Elaboration where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Numeric.Natural

--   _____             _
--  / ____|           | |
-- | (___  _   _ _ __ | |_ __ ___  __
--  \___ \| | | | '_ \| __/ _` \ \/ /
--  ____) | |_| | | | | || (_| |>  <
-- |_____/ \__, |_| |_|\__\__,_/_/\_\
--          __/ |
--         |___/

type Id = String
data Term = BVar Natural
          | FVar Id
          | MVar Id
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
        show (MVar x)    = '?' : x
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

mVarClose x t = go 0 x t
    where
        go k x t@(MVar y)  = if y == x then BVar k else t
        go k x (Lam ty t)  = Lam (go k x ty) (go (succ k) x t)
        go k x (App t1 t2) = App (go k x t1) (go k x t2)
        go k x (Pi ty t)   = Pi (go k x ty) (go (succ k) x t)
        go k x (Ite b ty t f) = Ite (go k x b) (go (succ k) x ty) (go k x t) (go k x f)
        go k x (IteL b t f) = IteL (go k x b) (go k x t) (go k x f)
        go k x (ExFalso e ty) = ExFalso (go k x e) (go k x ty)
        go _ _ t           = t

-- | Substitute a term for a free variable
subst x u  = open u . varClose x
msubst x u = open u . mVarClose x

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

isFullyElaborated (MVar _)       = False
isFullyElaborated (Lam ty t)     = isFullyElaborated ty && isFullyElaborated t
isFullyElaborated (App t1 t2)    = isFullyElaborated t1 && isFullyElaborated t2
isFullyElaborated (Pi ty t)      = isFullyElaborated ty && isFullyElaborated t
isFullyElaborated (Ite b ty t f) = all isFullyElaborated [b, ty, t, f]
isFullyElaborated (IteL b t f)   = all isFullyElaborated [b, t, f]
isFullyElaborated (ExFalso t ty) = isFullyElaborated t && isFullyElaborated ty
isFullyElaborated _              = True

--  ______ _       _                     _   _
-- |  ____| |     | |                   | | (_)
-- | |__  | | __ _| |__   ___  _ __ __ _| |_ _  ___  _ __
-- |  __| | |/ _` | '_ \ / _ \| '__/ _` | __| |/ _ \| '_ \
-- | |____| | (_| | |_) | (_) | | | (_| | |_| | (_) | | | |
-- |______|_|\__,_|_.__/ \___/|_|  \__,_|\__|_|\___/|_| |_|


type UniConstr     = (Term, Term, [Justif]) -- <t ~ s, j>
type ChoiceConstr  = (Id, Map Id Type, Type,
                     Term -> Type -> Substitution -> [[UniConstr]], [Justif])
                     -- <?m l : t in f, j>

data Justif = Asserted
            | Assumption
            deriving (Eq, Ord, Show)

type Assignment    = (Id, Term, Justif) -- ?m |-> <t, j>
type Substitution  = [Assignment]

class Constr c where
        apply :: Assignment -> c -> c

instance Constr UniConstr where
        apply (m, t, j) (u, v, j') = (msubst m t u, msubst m t v, j' ++ [j])

instance Constr ChoiceConstr where
        apply (m, s, j) (n, l, ty, f, j') =
            (n, l, msubst m s ty, f, j' ++ [j])

joins :: [UniConstr] -> Justif -> [UniConstr]
joins cs j = map f cs
    where f (t, s, j') = (t, s, j' ++ [j])

isReducible (App (Lam {}) _) = True
isReducible (Ite b _ _ _)    = b == TmTrue || b == TmFalse
isReducible (IteL b _ _)      = b == TmTrue || b == TmFalse
isReducible _                = False

reduce :: Term -> Term
reduce t = t

simp :: UniConstr -> Set UniConstr
simp (s, t, j)
    | s == t = S.empty
    | isReducible s = simp (reduce s, t, j)
    | isReducible t = simp (s, reduce t, j)
    -- ...

