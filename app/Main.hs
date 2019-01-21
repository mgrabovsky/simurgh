{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.IO.Class   (liftIO)
import Data.Foldable            (traverse_)
import System.Console.Haskeline
import System.Exit
import Text.Parsec              (parse)

import Simurgh.Syntax
import Simurgh.Parser (parseExpr)
import Simurgh.Pretty (prettyPrint)
import Simurgh.Typing (runTyping)
import Simurgh.Eval   (eval)

import Parser (replExpr)

data Command = Help
             | Type    String
             | Eval    String
             | Unknown String
             | Quit

-- TODO: Implement a proper parser for the REPL commands.

getCommand ("q", _)        = Quit
getCommand ("quit", _)     = Quit
getCommand ("?", _)        = Help
getCommand ("h", _)        = Help
getCommand ("help", _)     = Help
getCommand ("t", input)    = Type input
getCommand ("type", input) = Type input
getCommand (cmd, _)        = Unknown cmd

evaluate Help          = outputStrLn (
    "Available commands:\n" <>
    "    :?/h/help         Print this help message\n" <>
    "    :q/quit           Quit the REPL\n" <>
    "    :t/type <expr>    Infer the type of a term")
evaluate (Type input)  =
    case parseExpr input of
        Left (show -> error) -> outputStrLn $ "Parse error: " <> error
        Right expr ->
            case runTyping expr of
                Left error -> outputStrLn $ "Typing error: " <> error
                Right ty   -> outputStrLn (prettyPrint ty)
evaluate (Eval input)  =
    case parseExpr input of
        Left (show -> error) -> outputStrLn $ "Parse error: " <> error
        Right expr -> outputStrLn (prettyPrint (eval expr))
evaluate (Unknown cmd) = outputStrLn $ "Error: Unknown command '" <> cmd <> "'"
evaluate Quit          = liftIO exitSuccess

motd =
    "╒══════════════════════════════════╕\n" <>
    "│  Hello! Welcome to Simurgh REPL. │\n" <>
    "╘══════════════════════════════════╛\n"

repl = do
    line <- getInputLine "> "
    case line of
      Nothing    -> pure ()
      Just input ->
          case parse replExpr "<stdin>" input of
              Left _ -> evaluate (Eval input) >> repl
              Right (getCommand -> cmd) -> evaluate cmd >> repl

main :: IO ()
main = runInputT defaultSettings (outputStrLn motd >> repl)

