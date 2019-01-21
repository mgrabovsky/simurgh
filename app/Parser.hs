module Parser where

import           Control.Applicative  ((<|>))
import           Data.Functor         (($>))
import           Text.Parsec          hiding ((<|>), Empty)
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as P

replDef = emptyDef

lexer = P.makeTokenParser replDef

identifier = P.identifier lexer
colon      = P.colon lexer
whiteSpace = P.whiteSpace lexer

replExpr = (,) <$> (whiteSpace *> colon *> identifier) <*> many anyToken

