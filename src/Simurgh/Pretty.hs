module Simurgh.Pretty
    ( prettyPrint
    ) where

import qualified Data.Set             as S
import           Data.Set             (Set)
import           Data.Typeable (Typeable)
import qualified Text.PrettyPrint     as PP
import           Text.PrettyPrint     ((<+>), Doc)

import           Unbound.Generics.LocallyNameless
import           Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import Simurgh.Syntax

-- | Pretty printing for the new core syntax.

fvSet :: (Alpha a, Typeable b) => a -> Set (Name b)
fvSet = S.fromList . toListOf fv

class Pretty p where
    pprint :: (Applicative f, LFresh f) => p -> f Doc

instance Pretty Expr where
  pprint (Var x)     = pure (PP.text (show x))
  pprint (App t1 t2) = (\pt1 pt2 -> PP.parens (pt1 <+> pt2)) <$>
      pprint t1 <*> pprint t2
  pprint (Lam b)     = lunbind b $ \((x, Embed ty), body) -> do
      let argName = PP.text (show x)
          arr     = PP.text "=>"
      (\prettyTy prettyBody -> PP.text "λ" <+> PP.parens (argName <+> PP.colon <+> prettyTy)
                                   <+> arr <+> prettyBody) <$> pprint ty <*> pprint body
  pprint (Pi b)     = lunbind b $ \((x, Embed ty), body) -> do
      let argName = PP.text (show x)
          arr     = PP.text "=>"
      (\prettyTy prettyBody -> PP.text "Π" <+> PP.parens (argName <+> PP.colon <+> prettyTy)
                                <+> arr <+> prettyBody) <$> pprint ty <*> pprint body
  pprint (Let b)    = lunbind b $ \((x, Embed t), u) -> do
      let prefix = PP.text "let" <+> PP.text (show x) <+> PP.text "="
      ((\t u -> prefix <+> t <+> PP.text "in" <+> u) <$> pprint t <*> pprint u)
  pprint Set0        = pure (PP.text "Set")

prettyPrint :: Pretty a => a -> String
prettyPrint = PP.render . runLFreshM . pprint

