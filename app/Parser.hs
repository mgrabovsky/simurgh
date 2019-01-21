module Parser
    ( Command(..)
    , parseCommand
    ) where

import           Control.Applicative  ((<|>))
import           Data.Functor         (($>))
import           Text.Parsec          hiding ((<|>), Empty)
import           Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token    as P

import Command

getCommand "q"    _     = Quit
getCommand "quit" _     = Quit
getCommand "?"    _     = Help
getCommand "h"    _     = Help
getCommand "help" _     = Help
getCommand "t"    input = Type input
getCommand "type" input = Type input
getCommand cmd    _     = Unknown cmd

replDef = emptyDef
    { P.identLetter = letter <|> oneOf "_?"
    , P.identStart  = P.identLetter replDef
    }

lexer = P.makeTokenParser replDef

identifier = P.identifier lexer
colon      = P.colon lexer
whiteSpace = P.whiteSpace lexer

replExpr = whiteSpace *>
    (getCommand <$> (colon *> identifier) <*> many anyToken
    <|> Eval <$> many anyToken)

parseCommand = parse replExpr "<stdin>"

