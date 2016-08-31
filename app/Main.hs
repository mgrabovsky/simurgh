module Main where

import System.Console.Haskeline

import Lambda
import Parser

repl = do
        line <- getInputLine "> "
        case line of
            Nothing    -> pure ()
            Just input ->
                case parse "<stdin>" input of
                    Left e  -> outputStrLn ("Parsing error: " ++ show e)
                    Right t -> case inferType t of
                        Left e   -> outputStrLn ("Typing error: " ++ e)
                        Right ty -> outputStrLn (show ty)
                >> repl

main :: IO ()
main = runInputT defaultSettings repl

