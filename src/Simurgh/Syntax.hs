{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simurgh.Syntax
    ( Expr(..)
    , aeq
    , mkApp
    , mkLam
    , mkLet
    , mkPi
    , mkVar
    ) where

import Data.Monoid

import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Prelude       hiding (pi)

import Unbound.Generics.LocallyNameless

-- TODO: Syntax for modules, imports, data types, etc.
-- TODO: Start building a higher-level language with telescopes and other features in
-- the future. Translate into this core language in a pass, possibly multiple passes.

-- | The type of expressions (terms) of our core lambda calculus.
data Expr = Var  (Name Expr)
        -- ^ A variable is a name which can be substituted with an expression.
          | App   Expr Expr
        -- ^ Terms can be applied to one another. We only consider binary
        -- applications as it makes some things easier to implement and understand
        -- (and some harder).
          | Lam  (Bind (Name Expr, Embed Expr) Expr)
        -- ^ λ expressions bind a name with a given type inside their body.
          | Pi   (Bind (Name Expr, Embed Expr) Expr)
        -- ^ Pi expressions work similarly to λ expressions, but require the body to
        -- be a type and the resulting term is a type as well.
          | Let  (Bind (Name Expr, Embed Expr) Expr)
        -- ^ Local named bindings -- the familiar `let-in` construct.
          | Set0
        -- ^ The base type which is its own type (for now).
          deriving (Generic, Show, Typeable)

instance Alpha Expr

instance Subst Expr Expr where
    isvar (Var v) = Just (SubstName v)
    isvar _       = Nothing

-- Convenient helper functions.

mkVar :: String -> Expr
mkVar x = Var (string2Name x)

mkApp :: Expr -> [Expr] -> Expr
mkApp = foldl App

mkLam :: [(String, Expr)] -> Expr -> Expr
mkLam args body = foldr innerLam body args
  where innerLam (x, ty) body =
            Lam (bind (string2Name x, Embed ty) body)

mkLet :: String -> Expr -> Expr -> Expr
mkLet x t u = Let (bind (string2Name x, Embed t) u)

mkPi :: [(String, Expr)] -> Expr -> Expr
mkPi args body = foldr innerPi body args
  where innerPi (x, ty) body =
            Pi (bind (string2Name x, Embed ty) body)

