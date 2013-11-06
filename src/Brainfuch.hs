-- | Frontend for the Brainfuck interpreter.

module Brainfuch (runIO) where

import Compile  as C
import Parser   as P
import Optimize as O
import Types    as T


-- | Runs a given Brainfuck program in IO.
runIO :: String -> IO ()
runIO = runIO' . prepare
      where runIO' (Pure _) = return ()
            runIO' (Roll x) = case x of
                  ReadChar    cont -> getChar >>= runIO' . cont
                  PrintChar c cont -> putChar c >> runIO' cont


prepare :: String -> Program ()
prepare = C.compileBF . O.optimize . unsafeRight . P.parseBrainfuck
      where unsafeRight (Right x) = x
            unsafeRight (Left _)  = error "unsafeRight called on Left"