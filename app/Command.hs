module Command
    ( Command(..)
    ) where

data Command = Help
             | Type    String
             | Eval    String
             | Unknown String
             | Quit

