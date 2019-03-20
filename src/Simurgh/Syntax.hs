{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simurgh.Syntax
    ( Expr(..)
    , Telescope(..)
    , aeq
    , domain
    , dropTele
    , mkApp
    , mkLam
    , mkLet
    , mkPi
    , mkTelescope
    , mkVar
    ) where

import Data.Monoid

import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

import Unbound.Generics.LocallyNameless

-- TODO: Syntax for modules, imports, data types, etc.
-- TODO: Consider going back to unary binders, as many other things might be
-- simplified, most significantly partial application and subsequently related parts
-- of reduction and conversion. In that case, telescopes would only be part of the
-- higher-level syntax, which would be transformed into this core language in
-- a "preprocessing" pass.

-- | The type of expressions (terms) of our core lambda calculus.
data Expr = Var  (Name Expr)
        -- ^ A variable is a name which can be substituted with an expression.
          | App   Expr [Expr]
        -- ^ Terms can be applied to one another. We keep the "arguments" in a list
        -- in order to simplify semantics and interaction with telescopes.
          | Lam  (Bind Telescope Expr)
        -- ^ λ expression binds names in a telescope inside its body.
          | Pi   (Bind Telescope Expr)
        -- ^ Pi expression works similarly to λ expressions, but require the body to
        -- be a type.
          | Let  (Bind (Name Expr, Embed Expr) Expr)
        -- ^ Local named bindings -- the familiar `let-in` construct.
          | Set0
        -- ^ The base type universe. It is a member of itself for now.
          deriving (Generic, Show, Typeable)

data Telescope = Empty
               | Cons (Rebind (Name Expr, Embed Expr) Telescope)
               deriving (Generic, Show, Typeable)

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

-- Functions for working with telescopes.
-- TODO: Break out into a separate module.
domain :: Telescope -> [Name Expr]
domain Empty                               = []
domain (Cons (unrebind -> ((x, _), rest))) = x : domain rest

lengthTele :: Telescope -> Int
lengthTele Empty                          = 0
lengthTele (Cons (unrebind -> (_, rest))) = succ (lengthTele rest)

takeTele :: Int -> Telescope -> Telescope
takeTele 0 _                              = Empty
takeTele _ Empty                          = Empty
takeTele n (Cons (unrebind -> (b, rest))) =
    Cons (rebind b (takeTele (pred n) rest))

dropTele :: Int -> Telescope -> Telescope
dropTele 0 tele                           = tele
dropTele _ Empty                          = Empty
dropTele n (Cons (unrebind -> (_, rest))) = dropTele (pred n) rest

-- Convenient helper functions.

mkVar :: String -> Expr
mkVar x = Var (string2Name x)

mkApp :: Expr -> Expr -> Expr
mkApp t1 t2 = App t1 [t2]

mkLam :: [(String, Expr)] -> Expr -> Expr
mkLam xs t = Lam (bind (mkTelescope xs) t)

mkLet :: String -> Expr -> Expr -> Expr
mkLet x t u = Let (bind (string2Name x, Embed t) u)

mkPi :: [(String, Expr)] -> Expr -> Expr
mkPi xs t = Pi (bind (mkTelescope xs) t)

mkTelescope :: [(String, Expr)] -> Telescope
mkTelescope []             = Empty
mkTelescope ((x, t) : xs') = Cons (rebind (string2Name x, Embed t) (mkTelescope xs'))

