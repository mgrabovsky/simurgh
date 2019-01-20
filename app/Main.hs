module Main where

import Control.Monad.IO.Class   (liftIO)
import Data.Foldable            (traverse_)
import System.Console.Haskeline
import System.Exit

data Command = Help
             | Quit
             | Type    String
             | Eval    String
             | Unknown String

parseCommand ":q"                = Quit
parseCommand ":h"                = Help
parseCommand (':':'t':' ':input) = Type input
parseCommand (':':cmd)           = Unknown cmd
parseCommand input               = Eval input

evaluate Quit = liftIO exitSuccess
evaluate Help = outputStrLn (
    "Available commands:\n" <>
    "  :h    Print this help message\n" <>
    "  :q    Quit the REPL\n" <>
    "  :t    Infer the type of a term")
evaluate (Eval input) = outputStrLn "Type inference not yet implemented."
evaluate (Eval input) = outputStrLn "Term normalization not yet implemented."
evaluate (Unknown cmd) = outputStrLn $ "Error: Unknown command '" <> cmd <> "'"

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

