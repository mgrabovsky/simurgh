{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syntax where

import Control.Monad
import Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Monoid

import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Prelude       hiding (pi)

import Unbound.Generics.LocallyNameless

-- TODO: Syntax for modules, imports, etc.

-- | The type of expressions (terms) of our core lambda calculus.
data Expr = Var  (Name Expr)
        -- ^ A variable is a name which can be substituted with an expression.
          | App   Expr [Expr]
        -- ^ Terms can be applied to one another. We keep the "arguments" in a list
        -- in order to simplify semantics and interaction with telescopes.
          | Lam  (Bind Telescope Expr)
        -- ^ λ expressions bind names in a telescope inside its body.
          | Pi   (Bind Telescope Expr)
        -- ^ Pi expressions works similarly to λ expressions, but require the body to
        -- be a type.
          | Set0
        -- ^ The base type which is its own type (for now).
          deriving (Generic, Show, Typeable)

data Telescope = Empty
               | Cons (Rebind (Name Expr, Embed Expr) Telescope)
               deriving (Generic, Show, Typeable)

toList Empty                          = []
toList (Cons (unrebind -> (p, rest))) = p : toList rest

instance Semigroup Telescope where
  Empty                       <> t2 = t2
  Cons (unrebind -> (p, t1')) <> t2 = Cons (rebind p (t1' <> t2))

instance Monoid Telescope where
  mempty = Empty

instance Alpha Expr
instance Alpha Telescope

instance Subst Expr Expr where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing

instance Subst Expr Telescope

-- Convenient helper functions.

mkVar :: String -> Expr
mkVar x = Var (string2Name x)

mkApp :: Expr -> Expr -> Expr
mkApp t1 t2 = App t1 [t2]

mkLam :: [(String, Expr)] -> Expr -> Expr
mkLam xs t = Lam (bind (mkTelescope xs) t)

mkPi :: [(String, Expr)] -> Expr -> Expr
mkPi xs t = Pi (bind (mkTelescope xs) t)

mkTelescope :: [(String, Expr)] -> Telescope
mkTelescope []             = Empty
mkTelescope ((x, t) : xs') = Cons (rebind (string2Name x, Embed t) (mkTelescope xs'))

