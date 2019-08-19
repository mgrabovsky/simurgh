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

import Parser (Command(..), parseCommand)

evaluate Help          = outputStrLn (
    "Available commands:\n\n" <>
    "    :[h]elp           Print this help message\n" <>
    "    :[q]uit           Quit the REPL\n" <>
    "    :[t]ype <expr>    Infer the type of a term")
evaluate (Type input)  =
    case parseExpr input of
        Left (show -> err) -> outputStrLn $ "Parse error: " <> err
        Right expr ->
            case runTyping expr of
                Left err -> outputStrLn $ "Typing error: " <> err
                Right ty   -> outputStrLn (prettyPrint ty)
evaluate (Eval input)  =
    case parseExpr input of
        Left (show -> err) -> outputStrLn $ "Parse error: " <> err <>
                                            "\n\nType :help to see available commands."
        Right expr -> outputStrLn (prettyPrint (eval expr))
evaluate (Unknown cmd) = outputStrLn $ "Error: Unknown command '" <> cmd <> "'"
evaluate Quit          = liftIO exitSuccess

motd =
    "╒══════════════════════════════════════╕\n" <>
    "│  Hello! Welcome to the Simurgh REPL. │\n" <>
    "╘══════════════════════════════════════╛\n"

repl = do
    line <- getInputLine "> "
    case line of
      Nothing    -> pure ()
      Just input -> do
          case parseCommand input of
              Left (show -> err) -> outputStrLn err
              Right cmd -> evaluate cmd
          repl

main :: IO ()
main = runInputT settings (outputStrLn motd >> repl)
  where settings = (defaultSettings :: Settings IO) { complete = noCompletion }

