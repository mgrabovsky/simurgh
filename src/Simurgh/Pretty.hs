module Simurgh.Pretty
    ( prettyPrint
    ) where

import qualified Data.Set             as S
import           Data.Set             (Set)
import           Data.Typeable        (Typeable)
import qualified Text.PrettyPrint     as PP
import           Text.PrettyPrint     ((<+>), Doc, colon, comma, parens)
import           Unbound.Generics.LocallyNameless

import Simurgh.Syntax

-- TODO: Learn more about the individual tokens and combinators and how they can be
-- best used.
-- TODO: Pretty print `A -> B -> C` instaed of `Π(_:A)(_1:B), C`.
-- TODO: Be more liberal with regards to naked expressions -- fewer parentheses.

-- | Pretty printing for the new core syntax.

mapsto = PP.text "=>"

class Pretty p where
    pprint :: (Applicative f, LFresh f) => p -> f Doc

instance Pretty Expr where
  pprint (Var x)     = pure (PP.text (show x))
  pprint (App t1 ts) =
      let prettyTerms = traverse pprint (t1:ts)
       in parens <$> (foldr1 (<+>) <$> prettyTerms)
  pprint (Lam b)     = lunbind b $ \(tele, t) -> do
      prettyTele <- pprint tele
      ((PP.text "λ" <+> prettyTele <+> mapsto <+>) <$> pprint t)
  pprint (Pi b)     = lunbind b $ \(tele, t) -> do
      prettyTele <- pprint tele
      ((PP.text "Π" <+> (prettyTele <> comma) <+>) <$> pprint t)
  pprint (Let b)    = lunbind b $ \((x, Embed t), u) -> do
      let prefix = PP.text "let" <+> PP.text (show x) <+> PP.text "="
      ((\t u -> prefix <+> t <+> PP.text "in" <+> u) <$> pprint t <*> pprint u)
  pprint Set0        = pure (PP.text "Set")

instance Pretty Telescope where
  pprint Empty = pure PP.empty
  pprint (Cons (unrebind -> ((x, Embed t), rest))) =
      let ppVar  = PP.text (show x) <+> colon
          ppItem = (ppVar <+>) <$> pprint t
       in (<+>) <$> (parens <$> ppItem) <*> pprint rest

prettyPrint :: Pretty a => a -> String
prettyPrint = PP.render . runLFreshM . pprint

