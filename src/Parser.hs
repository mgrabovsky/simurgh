module Parser where

import Control.Monad.Reader
import Data.Functor (($>))
import Data.List (elemIndex)
import Prelude hiding (const, pi)
import Text.Parsec hiding (parse)
import Text.Parsec.Language (LanguageDef)
import qualified Text.Parsec.Token as P

import Lambda

type Parser = ParsecT String () (Reader [Id])

-- Built-in constants
const = choice [ reserved "Set"   $> TySet
               , reserved "empty" $> TyEmpty
               , reserved "unit"  $> TyUnit
               , reserved "bool"  $> TyBool
               , reserved "tt"    $> TmUnit
               , reserved "true"  $> TmTrue
               , reserved "false" $> TmFalse
               ]
    <?> "constant"

-- Eliminators for Booleasn and the empty type
-- TODO: Implement the 'as .. return ...' syntax for proper dependent elimination
elim = Ite <$> (reserved "if" >> naked) <*> (reservedOp "/" >> naked) <*>
                (reserved "then" >> naked) <*> (reserved "else" >> naked)
   <|> IteL <$> (reserved "If" >> naked) <*> (reserved "Then" >> naked) <*>
               (reserved "Else" >> naked)
   <|> ExFalso <$> (reserved "exfalso" >> naked) <*> (reservedOp "!" >> naked)


nonApp = const
    <|> parens expr
    <|> makeVar =<< identifier
    where
        makeVar :: Id -> Parser Term
        makeVar x = do
            vars <- ask
            case elemIndex x vars of
                Just n -> pure (BVar (fromIntegral n))
                _      -> pure (FVar x)

naked = chainl1 nonApp (spaces >> pure App)

-- Type annotations (for use in binders etc.)
annotated = (,) <$> identifier <*> (colon >> naked)

lambda = reserved "fun" >> (plain <|> poly)
    where
        plain = do
            (ident, ty) <- annotated
            reservedOp "=>"
            body <- local (ident :) expr
            pure (Lam ty body)
        poly = do
            (ident, ty) <- parens annotated
            (idents, tys) <- unzip <$> local (ident :) binders
            reservedOp "=>"
            body <- local (\x -> foldl (flip (:)) x (ident:idents)) expr
            pure (foldr Lam body (ty:tys))

binders = do
    opt <- optionMaybe $ parens annotated
    case opt of
        Just (ident, ty) -> do
            xs <- local (ident :) binders
            pure ((ident, ty):xs)
        _ -> pure []

prase' :: Parser a -> String -> Either ParseError a
prase' p s = runReader (runParserT p () "<stdin>" s) []

binder kw sep ctor = do
        reserved kw
        (ident, ty) <- annotated
        reservedOp sep
        body <- local (ident :) expr
        pure (ctor ty body)

pi = reserved "forall" >> (plain <|> poly)
    where
        plain = do
            (ident, ty) <- annotated
            reservedOp ","
            body <- local (ident :) expr
            pure (Pi ty body)
        poly = do
            (ident, ty) <- parens annotated
            (idents, tys) <- unzip <$> local (ident :) binders
            reservedOp ","
            body <- local (\x -> foldl (flip (:)) x (ident:idents)) expr
            pure (foldr Pi body (ty:tys))


expr :: Parser Term
expr = lambda
   <|> pi
   <|> elim
   <|> naked

parse filePath input = runReader (runParserT (skipMany space *> expr <* eof) () filePath input) []

{-
 - The lexer
 -}
simurghDef = P.LanguageDef
           { P.commentStart    = ""
           , P.commentEnd      = ""
           , P.commentLine     = ""
           , P.nestedComments  = True
           , P.identStart      = letter <|> char '_'
           , P.identLetter     = alphaNum <|> oneOf "_'"
           , P.opStart         = P.opLetter simurghDef
           , P.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , P.reservedNames   = ["fun", "forall", "Set", "empty", "unit", "bool",
                                  "tt", "true", "false", "if", "then", "else",
                                  "If", "Then", "Else", "exfalso"]
           , P.reservedOpNames = [":", "=>", "!", "/"]
           , P.caseSensitive   = True
           }


lexer = P.makeTokenParser simurghDef

colon = P.colon lexer
identifier = P.identifier lexer
parens = P.parens lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

