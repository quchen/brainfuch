module Main (main) where

import System.Environment (getArgs)

import qualified Brainfuch as Brainfuck


-- | Read from STDIN when no argument is specified, otherwise
main :: IO ()
main = do
      args <- getArgs
      case args of
            ("--help":_) -> showHelp
            ("-h":_)     -> showHelp
            []           -> getContents >>= Brainfuck.run
            (h:_)        -> readFile h >>= Brainfuck.run


showHelp :: IO ()
showHelp = do
      putStrLn "Usage: ./brainfuch [source]"
      putStrLn "If no source is specified, read from STDIN"