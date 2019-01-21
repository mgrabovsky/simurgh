module Simurgh.Parser
    ( parseExpr
    ) where

import           Prelude              hiding (pi)

import           Control.Applicative  ((<|>))
import           Data.Functor         (($>))
import           Text.Parsec          hiding ((<|>), Empty)
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as P

import Simurgh.Syntax

-- TODO: Parse modules, imports, declarations, etc.

-- | A parser for the new Unbound-based core syntax representation.

-- | Lexer definition
simurghDef = emptyDef
    { P.commentStart    = "{-"
    , P.commentEnd      = "-}"
    , P.commentLine     = "--"
    , P.nestedComments  = True
    , P.reservedNames   = ["forall", "fun"]
    , P.reservedOpNames = ["=>"]
    }

lexer      = P.makeTokenParser simurghDef
colon      = P.colon lexer
ident      = P.identifier lexer
parens     = P.parens lexer
reserved   = P.reserved lexer
whiteSpace = P.whiteSpace lexer

arrow = P.reservedOp lexer "=>"

expr = do
    applicand <- atom
    args      <- many atom
    if null args
       then pure applicand
       else pure (App applicand args)

atom = parens expr
    <|> reserved "Set" $> Set0
    <|> mkVar <$> ident
    <|> mkLam <$> (reserved "fun" *> binders <* arrow)
        <*> expr
    <|> mkPi <$> (reserved "forall" *> binders <* arrow)
        <*> expr

binders = many1 binder

binder = parens ((,) <$> ident <*> (colon *> expr))

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace *> expr) "<stdin>"

