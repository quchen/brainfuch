-- | Frontend for the Brainfuck interpreter.

module Brainfuch (run) where

import Superfuck
import Parser


-- | Runs a given Brainfuck program.
run :: String -> IO ()
run source = case parseBrainfuck source of
      Right s -> runSuperfuck s
      Left  e -> print e

