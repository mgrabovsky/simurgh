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
  pprint (App t1 ts) =
      let prettyTerms = traverse pprint (t1:ts)
       in PP.parens <$> (foldr1 (<+>) <$> prettyTerms)
  pprint (Lam b)     = lunbind b $ \(tele, t) -> do
      prettyTele <- pprint tele
      ((PP.text "λ" <+> prettyTele <+> PP.text "=>" <+>) <$> pprint t)
  pprint (Pi b)     = lunbind b $ \(tele, t) -> do
      prettyTele <- pprint tele
      ((PP.text "Π" <+> prettyTele <+> PP.text "=>" <+>) <$> pprint t)
  pprint Set0        = pure (PP.text "Set")

instance Pretty Telescope where
  pprint Empty = pure PP.empty
  pprint (Cons (unrebind -> ((x, unembed -> t), rest))) =
      let ppVar  = PP.text (show x) <+> PP.colon 
          ppItem = (ppVar <+>) <$> pprint t
       in (<+>) <$> (PP.parens <$> ppItem) <*> pprint rest

prettyPrint :: Pretty a => a -> String
prettyPrint = PP.render . runLFreshM . pprint

