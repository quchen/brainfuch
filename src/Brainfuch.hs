-- | Frontend for the Brainfuck interpreter.

{-# LANGUAGE FlexibleContexts #-}

module Brainfuch (runIO, runString) where

import Compile  as C
import Optimize as O
import Parser   as P
import Types    as T

import Control.Monad.Writer
import Data.DList
import System.IO            as IO



-- | Runs a given Brainfuck program in IO.
runIO :: String  -- ^ Source
      -> IO ()
runIO = runIO' . prepare
  where
    runIO' (Pure _) = pure ()
    runIO' (Roll x) = case x of
        ReadChar    cont -> IO.getChar >>= runIO' . cont
        PrintChar c cont -> IO.putChar c >> runIO' cont

runString :: String
          -> String
          -> String
runString input source = (toList . execWriter . runW input . prepare) source

  where -- Run using Writer+DList

    runW _ (Pure _) = pure ()
    runW (i:is) (Roll (ReadChar cont)) = runW is (cont i)
    runW is (Roll (PrintChar c cont)) = do
          tell (singleton c)
          runW is cont
    runW [] (Roll (ReadChar _cont)) = tell (fromList "<unexpected end of input>")

prepare :: String -> Program ()
prepare s = (C.compileBF . O.optimize . unsafeRight . P.parseBrainfuck) s
  where
    unsafeRight (Right x) = x
    unsafeRight (Left err)  = error ("unsafeRight called on Left(" ++ show err ++ ")")
