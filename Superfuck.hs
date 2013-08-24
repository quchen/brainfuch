{-# LANGUAGE BangPatterns #-}

-- | Superfuck is an intermediate language based on Brainfuck, but has an easier
--   syntax representation, which makes it more suitable for optimizations.
--
--     * Multiple uses of +, -, >, < can be combined, for example "+(3)" stands
--       for "+++".
--     * Consecutive comment characters are combined into a comment String
--       (Brainfuck.hs treats every character on its own).
--
--   This enables a couple of optimizations to be dealt with easily, for
--   example:
--
--     * Consecutive + and -, < and > combine/cancel
--     * Adding X to a cell can be done in a single arithmetic operation instead
--       of adding 1 repeatedly
--     * Printing the same character multiple times can be done by a single
--       output statement

module Superfuck (
      runSuperfuck
) where

import Data.Char (chr, ord)
import Data.Monoid
import Control.Monad

import qualified Stream as S
import Comonad
import Tape
import Utilities
import Types




-- | Executes a Superfuck program (as given; if optimizations are desired apply
--   them explicitly).
runSuperfuck :: SuperfuckSource -> IO ()
runSuperfuck = void . run S.emptyTape


-- | Apply an endomorphism multiple times
times :: Int
      -> (a -> a)
      -> (a -> a)
times n f = appEndo . mconcat . map Endo $ replicate n f



-- | Execute the command at the current location of the instruction tape
run :: Tape S.Stream Int -- ^ Data tape
    -> SuperfuckSource   -- ^ Instruction tape
    -> IO (Tape S.Stream Int)
run tape (SFSource []) = return tape
run tape@(Tape l !p r) (SFSource (x:xs)) = let rest = SFSource xs
                                           in  case x of

      Move n -> run (abs n `times` f $ tape) rest
            where f | n > 0 = S.focusRight
                    | n < 0 = S.focusLeft
                    | otherwise = error "'Move 0' encountered, bug"
                        -- ^ "id" would be semantically correct here, but there
                        --   should really be a red loud bug warning at this
                        --   time. (TODO: remove this case when everything's
                        --   polished)

      Add n -> run (Tape l (p+n) r) rest

      Print n -> do putStr (replicate (fromIntegral n) (chr $ p `mod` 128))
                    flush
                    run tape rest
      Read    -> do c <- getChar
                    run (Tape l (ord c) r) rest

      -- Loop
      Loop body
            | p == 0    -> run tape rest -- ignore entire loop
            | otherwise -> do tape' <- runLoop tape body
                              run tape' rest


runLoop :: Tape S.Stream Int -- ^ Data tape
        -> SuperfuckSource   -- ^ Instruction tape
        -> IO (Tape S.Stream Int)
runLoop tape body = do
      tape' <- run tape body
      if extract tape' /= 0
            then runLoop tape' body
            else return tape'
