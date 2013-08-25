-- | Frontend for the Brainfuck interpreter.

module Brainfuch (run) where

import Brainfuck
import Parser


-- | Runs a given Brainfuck program.
run :: String -> IO ()
run = either print runBrainfuck . parseBrainfuck
