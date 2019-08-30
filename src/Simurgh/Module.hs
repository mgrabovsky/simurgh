{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simurgh.Module
    ( Module
    , ModuleItem(..)
    ) where

import Data.Typeable                    (Typeable)
import GHC.Generics                     (Generic)
import Unbound.Generics.LocallyNameless

import Simurgh.Syntax

-- Convenient type synonym.
type Type = Expr

data Module = Module (TRec [ModuleItem])
            deriving (Generic, Show, Typeable)
-- FIXME: This is not the binding structure we want. The names should not bind
-- insides their types. (Altough they may bind inside the body.)
data ModuleItem = Axiom (Name Expr) (Embed Type)
                | Defn  (Name Expr) (Embed Type) (Embed Expr)
                deriving (Generic, Show, Typeable)

instance Alpha Module
instance Alpha ModuleItem
instance Subst Expr Module
instance Subst Expr ModuleItem

bindingTest = Module (trec [axA]) where
    axA    = Axiom (string2Name "A") (Embed (mkPi [("_", App (mkVar "f") [mkVar "A"])]
                                                  Set0))
    trec p = TRec (bind (rec p) ())

moduleTest = Module (trec [bool, true, false, bneg, band, bor]) where
    -- axiom bool : Set
    bool  = Axiom (string2Name "bool") (Embed Set0)
    -- axiom true : bool
    true  = Axiom (string2Name "true") (Embed (mkVar "bool"))
    -- axiom false : bool
    false = Axiom (string2Name "false") (Embed (mkVar "bool"))
    -- axiom bneg : bool -> bool -> bool
    bneg  = Axiom (string2Name "bneg") (Embed (mkPi [("_", mkVar "bool")] (mkVar "bool")))
    -- axiom band : bool -> bool -> bool
    band  = Axiom (string2Name "band") (Embed (mkPi [("_", mkVar "bool"),
                                                     ("_", mkVar "bool")]
                                                    (mkVar "bool")))
    -- def bor : bool -> bool -> bool
    --         := fun (x y : bool) => band (bneg x) (bneg y)
    --
    -- In the future:
    -- def bor (x y : bool) : bool := band (bneg x) (bneg y)
    --
    -- In a far future future:
    -- def bor x y := band (bneg x) (bneg y)
    bor   = Defn  (string2Name "bor")
                  (Embed (mkPi [("_", mkVar "bool"), ("_", mkVar "bool")]
                               (mkVar "bool")))
                  (Embed $ mkLam [("x", mkVar "bool"), ("y", mkVar "bool")]
                                 (App (mkVar "band")
                                      [App (mkVar "bneg") [mkVar "x"],
                                       App (mkVar "bneg") [mkVar "y"]]))
    trec p = TRec (bind (rec p) ())

