module Main (main) where

import System.Environment
import System.IO

import qualified Brainfuch as Brainfuck


-- | Read from STDIN when no argument is specified, otherwise
main :: IO ()
main = do
      args <- getArgs
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin  NoBuffering
      case args of
            ("--help":_) -> showHelp
            ("-h":_)     -> showHelp
            []           -> getContents   >>= Brainfuck.run
            (file:_)     -> readFile file >>= Brainfuck.run


showHelp :: IO ()
showHelp = do
      putStrLn "Usage: ./brainfuch [source]"
      putStrLn "If no source is specified, read from STDIN"