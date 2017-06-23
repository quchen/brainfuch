module Main (main) where



import qualified Data.Text.IO       as T
import           System.Environment
import           System.IO

import qualified Brainfuch



-- | Read from STDIN when no argument is specified, otherwise
main :: IO ()
main = do
      args <- getArgs
      hSetBuffering stdout NoBuffering
      hSetBuffering stdin  NoBuffering
      case args of
            ("--help":_)   -> showHelp
            ("-h":_)       -> showHelp
            []             -> T.getContents >>= Brainfuch.runIO
            (file:_)       -> T.readFile file >>= Brainfuch.runIO

showHelp :: IO ()
showHelp = do
      putStrLn "Read from file:         ./brainfuch <source>"
      putStrLn "Read from STDIN:        ./brainfuch"
