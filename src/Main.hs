module Main (main) where

import System.Environment
import System.IO

import qualified Brainfuch


-- | Read from STDIN when no argument is specified, otherwise
main :: IO ()
main = do
      args <- getArgs
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin  NoBuffering
      case args of
            ("--help":_) -> showHelp
            ("-h":_)     -> showHelp
            []           -> getContents   >>= Brainfuch.runIO
            (file:_)     -> readFile file >>= Brainfuch.runIO


showHelp :: IO ()
showHelp = do
      putStrLn "Usage: ./brainfuch [source]"
      putStrLn "If no source is specified, read from STDIN"