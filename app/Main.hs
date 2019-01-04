module Main where

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.Exit

import           Lambda
import qualified Parser

data Command = Help
             | Quit
             | Type String
             | Eval String

parse ":q"  = Quit
parse ":h"  = Help
parse (':':'t':' ':input) = Type input
parse input = Eval input

evaluate Quit = liftIO exitSuccess
evaluate Help = outputStrLn (
    "Available commands:\n" <>
    "  :h    Print this help message\n" <>
    "  :q    Quit the REPL\n" <>
    "  :t    Infer the type of a term")
evaluate (Type input) =
    case Parser.parse "<stdin>" input of
      Left  e -> outputStrLn ("Parsing error: " ++ show e)
      Right t ->
          case inferType t of
            Left  e  -> outputStrLn ("Typing error: " ++ e)
            Right ty -> outputStrLn (show ty)
evaluate (Eval input) = outputStrLn "Term normalization not yet implemented."

motd =
    "/////////////////////////////////\n" <>
    " Hello! Welcome to Simurgh REPL. \n" <>
    "/////////////////////////////////\n"

repl = do
    line <- getInputLine "> "
    case line of
      Nothing    -> pure ()
      Just input ->
          evaluate (parse input) >> repl

main :: IO ()
main = runInputT defaultSettings (outputStrLn motd >> repl)

