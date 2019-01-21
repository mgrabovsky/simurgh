{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.IO.Class   (liftIO)
import Data.Foldable            (traverse_)
import System.Console.Haskeline
import System.Exit

import Syntax
import Parser (parseExpr)
import Pretty (prettyPrint)
import Typing (runTyping)
import Eval   (eval)

data Command = Help
             | Type    String
             | Eval    String
             | Unknown String
             | Quit

-- TODO: Implement a proper parser for the REPL commands.

parseCommand ":q"                = Quit
parseCommand ":h"                = Help
parseCommand (':':'t':' ':input) = Type input
parseCommand (':':cmd)           = Unknown cmd
parseCommand input               = Eval input

evaluate Help          = outputStrLn (
    "Available commands:\n" <>
    "  :h    Print this help message\n" <>
    "  :q    Quit the REPL\n" <>
    "  :t    Infer the type of a term")
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
          evaluate (parseCommand input) >> repl

main :: IO ()
main = runInputT defaultSettings (outputStrLn motd >> repl)

