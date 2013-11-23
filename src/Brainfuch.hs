-- | Frontend for the Brainfuck interpreter.

module Brainfuch (runIO, runString) where

import Compile  as C
import Parser   as P
import Optimize as O
import Types    as T

import Data.DList
import Control.Monad.Writer
import System.IO as IO

import Debug.Trace


-- | Runs a given Brainfuck program in IO.
runIO :: String  -- ^ Source
      -> IO ()
runIO = runIO' . prepare
      where runIO' (Pure _) = return ()
            runIO' (Roll x) = case x of
                  ReadChar    cont -> IO.getChar >>= runIO' . cont
                  PrintChar c cont -> IO.putChar c >> runIO' cont



runString :: String -- ^ Program input
          -> String -- ^ Source
          -> String
runString input = toList . execWriter . runW input . prepare

      where -- Run using Writer+DList

            runW _ (Pure _) = do
                  return ()

            runW (i:is) (Roll (ReadChar cont)) = do
                  runW is (cont i)

            runW is (Roll (PrintChar c cont)) = do
                  tell (singleton c)
                  runW is cont

            runW [] (Roll (ReadChar _cont)) = do
                  tell (fromList "<unexpected end of input>")



prepare :: String -> Program ()
prepare = C.compileBF . O.optimize . unsafeRight . P.parseBrainfuck
      where unsafeRight (Right x) = x
            unsafeRight (Left _)  = error "unsafeRight called on Left"