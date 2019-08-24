module Simurgh.Parser
    ( parseExpr
    ) where

import           Control.Applicative  ((<|>))
import           Data.Functor         (($>))
import           Text.Parsec          hiding ((<|>), Empty)
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as P

import Simurgh.Syntax

-- TODO: Parse modules, imports, declarations, etc.
-- TODO: Consider switching to ByteString and parsing Unicode syntax as well.
-- For instance, → ⇒ λ ∀ Π.

-- | A parser for the new Unbound-based core syntax representation.

-- | Lexer definition
simurghDef = emptyDef
    { P.commentStart    = "{-"
    , P.commentEnd      = "-}"
    , P.commentLine     = "--"
    , P.nestedComments  = True
    , P.reservedNames   = ["forall", "fun", "in", "let"]
    , P.reservedOpNames = ["->", "=>", "="]
    }

lexer      = P.makeTokenParser simurghDef
colon      = P.colon lexer
comma      = P.comma lexer
ident      = P.identifier lexer
parens     = P.parens lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
whitespace = P.whiteSpace lexer

arrow      = reservedOp "->"
mapsto     = reservedOp "=>"

-- | Parser for an expression of the core lambda calculus.
app = do
    applicand <- naked
    args      <- many naked
    if null args
       then pure applicand
       else pure (App applicand args)

mkArrow t1 ts = let bs   = init (t1:ts)
                    body = last ts
                 in mkPi ((,) "_" <$> bs) body
-- FIXME: It's not safe to bind a variable named `_` here.

expr =  -- For expressions such as `A x y -> B -> Set`.
        -- TODO: Create tests for this syntax.
        try (mkArrow <$> app <*> (arrow *> sepBy1 expr arrow))
    <|> app
    <|> mkLam <$> (reserved "fun" *> binders <* mapsto)
              <*> expr
    <|> mkPi <$> (reserved "forall" *> binders <* comma)
             <*> expr
    <|> mkLet <$> (reserved "let" *> ident)
              <*> (reservedOp "=" *> expr)
              <*> (reserved "in" *> expr)

-- | Parser for an atomic expression of the language, i.e. the @Set@ literal,
-- a variable name, lambda abstraction, Pi type, the @let-in@ construct or
-- a parenthesised expression.
naked =  parens expr
     <|> reserved "Set" $> Set0
     <|> mkVar <$> ident

-- TODO: Support syntax like `fun (x y : A) => _` for `fun (x : A) (y : A) => _`
-- NOTE: Mind expressions like `fun (A:Set) (A B:A) => Set`, which should be invalid.
-- TODO: Parse `A -> B -> C` into `forall (_:A) (_:B) => C`,
-- perhaps even Agda/Idris-like `(a:A) -> (b:B) -> C a b` for
-- `forall (a:A) (b:B) => C A b`

-- | Parser for a telescope of binders.
binders = many1 binder

-- | Parser for a single binder of the form @(x : A)@.
binder = parens ((,) <$> ident <*> (colon *> expr))

-- | Parse an expression of the core lambda calculus and return the result. The input
-- file name is presumed to be "<stdin>".
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whitespace *> expr <* eof) "<stdin>"

