module Main (main) where



import Data.List
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
            ("--help":_)   -> showHelp
            ("-h":_)       -> showHelp
            []             -> getContents >>= Brainfuch.runIO
            xs@((':':_):_) -> print (unsafeFromText xs)
            (file:_)       -> readFile file >>= Brainfuch.runIO

showHelp :: IO ()
showHelp = do
      putStrLn "Read from file:         ./brainfuch <source>"
      putStrLn "Read from command line: ./brainfuch :<source>:<program input>"
      putStrLn "Read from STDIN:        ./brainfuch"

-- | Used to run brainfuck programs given using only command line parameters.
unsafeFromText :: [String] -> String
unsafeFromText xs =
      let (source, input) = splitOn ':' . drop 1 $ intercalate " " xs
      in  Brainfuch.runString input source

-- | Split a list on a certain element. The splitting element is dropped.
splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn c xs = let (before, _:after) = break (== c) xs
               in (before, after)
