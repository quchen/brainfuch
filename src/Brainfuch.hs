-- | Frontend for the Brainfuck interpreter.

{-# LANGUAGE FlexibleContexts #-}



module Brainfuch (runIO, runText) where

import           Control.Monad.Writer
import           Data.DList
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           System.IO            as IO

import Compile  as C
import Optimize as O
import Parser   as P
import Types    as T



-- | Runs a given Brainfuck program in IO.
runIO :: Text  -- ^ Source
      -> IO ()
runIO = runIO' . prepare
  where
    runIO' (Pure _) = pure ()
    runIO' (Roll x) = case x of
        ReadChar    cont -> IO.getChar >>= runIO' . cont
        PrintChar c cont -> IO.putChar c >> runIO' cont

runText :: Text
          -> Text
          -> TL.Text
runText input source = (TL.pack . toList . execWriter . runW input . prepare) source

  where -- Run using Writer+DList

    runW _ (Pure _) = pure ()
    runW is (Roll (ReadChar cont))
        | Just (i,is') <- T.uncons is = runW is' (cont i)
        | otherwise = tell (fromList "<unexpected end of input>")
    runW is (Roll (PrintChar c cont)) = do
          tell (singleton c)
          runW is cont

prepare :: Text -> Program ()
prepare s = (C.compileBF . O.optimize . unsafeFromJust . P.parseBrainfuck) s
  where
    unsafeFromJust (Just x) = x
    unsafeFromJust Nothing  = error "unsafeFromJust called on Nothing"
