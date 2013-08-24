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
import Tape
import Utilities
import Types




-- TODO: Add optimizer again. The parser will remove one pass of redundant
--       operations only, so "+-.+-.+-." will result in "...", but should be
--       ".3".
--
--       - Remove loops that are never entered (beginning of file, or loop
--         immediately after another loop)




-- | Executes a Superfuck program (as given; if optimizations are desired apply
--   them explicitly).
runSuperfuck :: SuperfuckSource -> IO ()
runSuperfuck source = void $ run source S.emptyTape


-- | Apply an endomorphism multiple times
times :: Int
      -> (a -> a)
      -> (a -> a)
times n f = appEndo . mconcat . map Endo $ replicate n f



-- | Execute the command at the current location of the instruction tape
run :: SuperfuckSource        -- ^ Instruction tape
    -> Tape S.Stream Int      -- ^ Data tape
    -> IO (Tape S.Stream Int) -- ^ Tape after termination
run (SFSource []) tape = return tape
run (SFSource (x:xs)) tape@(Tape l !p r) = let rest = SFSource xs
                                           in  case x of

      Move n -> run rest (abs n `times` f $ tape)
            where f | n > 0 = S.focusRight
                    | n < 0 = S.focusLeft
                    | otherwise = error "'Move 0' encountered, bug"
                        -- ^ "id" would be semantically correct here, but there
                        --   should really be a red loud bug warning at this
                        --   time. (TODO: remove this case when everything's
                        --   polished)

      Add n -> run rest (Tape l (p+n) r)

      Print n -> do putStr (replicate (fromIntegral n) (chr $ p `mod` 2^8))
                    flush
                    run rest tape
      Read    -> do c <- getChar
                    run rest (Tape l (ord c) r)

      -- Loop
      Loop body
            | p == 0    -> run rest tape  -- ignore entire loop
            | otherwise -> runLoop body tape >>= run rest


runLoop :: SuperfuckSource   -- ^ Instruction tape
        -> Tape S.Stream Int -- ^ Data tape
        -> IO (Tape S.Stream Int)
runLoop body tape = do
      tape'@(Tape _ p _) <- run body tape
      if p /= 0
            then runLoop body tape'
            else return tape'
