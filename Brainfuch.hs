-- | Frontend for the Brainfuck interpreter.

module Brainfuch (runBrainfuck) where

import Brainfuck (parseBrainfuck)
import Superfuck (runSuperfuck, optimize, bf2sf)

-- | Runs a given Brainfuck program.
runBrainfuck :: String -> IO ()
runBrainfuck = runSuperfuck . optimize . bf2sf . parseBrainfuck



