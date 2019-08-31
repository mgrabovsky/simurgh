{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Simurgh.Module
    ( Module
    , Entity(..)
    ) where

import Data.Map      (Map, fromAscList, singleton)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Prelude       hiding (pi)

import Unbound.Generics.LocallyNameless

import Simurgh.Syntax

-- Convenient type synonym.
type Type = Expr

data Module = Module (Map String Entity)
            deriving (Generic, Show, Typeable)
data Entity = Axiom Type
            | Defn  Type {- ← ? -} Expr
            deriving (Generic, Show, Typeable)

-- A couple of convenient shortcuts.
var = mkVar
pi  = mkPi

infixl 1 @@
(@@) :: Expr -> Expr -> Expr
App e₁ es @@ e₂ = App e₁ (es ++ [e₂])
e₁        @@ e₂ = App e₁ [e₂]

infixr 1 -->
(-->) :: Type -> Type -> Type
t₁ --> t₂    = pi [("_", t₁)] t₂
-- t₁ --> Pi ts = Pi (bind (Cons (rebind ("_", t₁) ts))) -- Not that easy.

bindingTest = Module (singleton "A" axA) where
    axA = Axiom ((var "f" @@ mkVar "A") --> Set0)

moduleTest = Module (fromAscList [("band",  band),
                                  ("bneg",  bneg),
                                  ("bool",  bool),
                                  ("bor",   bor),
                                  ("false", false),
                                  ("true",  true)])
  where
    -- axiom bool : Set
    bool  = Axiom Set0
    -- axiom true : bool
    true  = Axiom (var "bool")
    -- axiom false : bool
    false = Axiom (var "bool")
    -- axiom bneg : bool -> bool -> bool
    bneg  = Axiom (var "bool" --> var "bool")
    -- axiom band : bool -> bool -> bool
    band  = Axiom (var "bool" --> var "bool" --> var "bool")
    -- def bor : bool -> bool -> bool
    --         := fun (x y : bool) => band (bneg x) (bneg y)
    --
    -- In the future:
    -- def bor (x y : bool) : bool := band (bneg x) (bneg y)
    --
    -- In a far future future:
    -- def bor x y := band (bneg x) (bneg y)
    bor   = Defn (var "bool" --> var "bool" --> var "bool")
                 (mkLam [("x", var "bool"), ("y", var "bool")]
                        (var "band" @@ (var "bneg" @@ var "x")
                                    @@ (var "bneg" @@ var "y")))

