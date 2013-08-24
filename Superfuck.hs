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
run tape@(Tape l !p r) source@(SFSource (x:_)) = case x of

      Move n -> advance (abs n `times` f $ tape) source
            where f | n > 0 = S.focusRight
                    | n < 0 = S.focusLeft
                    | otherwise = error "'Move 0' encountered, bug"
                        -- ^ "id" would be semantically correct here, but there
                        --   should really be a red loud bug warning at this
                        --   time. (TODO: remove this case when everything's
                        --   polished)

      Add n -> advance (Tape l (p+n) r) source

      Print n -> do putStr (replicate (fromIntegral n) (chr p))
                    flush
                    advance tape source
      Read    -> do c <- getChar
                    advance (Tape l (ord c) r) source

      -- Loop
      Loop body
            | p == 0    -> advance tape source -- ignore entire loop
            | otherwise -> do tape' <- runLoop tape body
                              advance tape' source


runLoop :: Tape S.Stream Int -- ^ Data tape
        -> SuperfuckSource   -- ^ Instruction tape
        -> IO (Tape S.Stream Int)
runLoop tape body = do
      tape' <- run tape body
      if extract tape' /= 0
            then runLoop tape' body
            else return tape'


-- Advance instruction pointer (or terminate)
advance :: Tape S.Stream Int -- ^ Data tape
        -> SuperfuckSource   -- ^ Instruction tape
        -> IO (Tape S.Stream Int)
advance tape (SFSource [])     = return tape
advance tape (SFSource (_:xs)) = run tape (SFSource xs)
