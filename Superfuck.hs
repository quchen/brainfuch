{-# LANGUAGE BangPatterns #-}

-- | Superfuck is the internal representation for Brainfuck, more suitable
--   for optimizations than a direct encoding.

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
runSuperfuck = void . flip run S.emptyTape





-- | Apply an endomorphism multiple times
times :: Int
      -> (a -> a)
      -> (a -> a)
times n f = appEndo . mconcat . map Endo $ replicate n f





-- | Execute the command at the current location of the instruction tape
run :: SuperfuckSource        -- ^ Instruction tape
    -> Tape S.Stream Int      -- ^ Data tape
    -> IO (Tape S.Stream Int) -- ^ Tape after termination

run (SFSource []    ) tape               = return tape
run (SFSource (x:xs)) tape@(Tape l !p r) = let rest = SFSource xs
                                           in  case x of

      Move n -> run rest (n' `times` f $ tape)
            where n' = abs n
                  f | n < 0     = S.focusLeft
                    | otherwise = S.focusRight

      Add n -> run rest (Tape l (p+n) r)

      Print n -> do putStr $ replicate (fromIntegral n) (chr $ p `mod` 2^8)
                    flush
                    run rest tape

      Read -> do c <- getChar
                 run rest (Tape l (ord c) r)

      Loop body | p == 0    -> run rest tape  -- ignore entire loop
                | otherwise -> runLoop body tape >>= run rest





runLoop :: SuperfuckSource        -- ^ Instruction tape
        -> Tape S.Stream Int      -- ^ Data tape
        -> IO (Tape S.Stream Int) -- ^ Tape after the loop terminates

runLoop body tape = do
      tape'@(Tape _ p _) <- run body tape
      case p of
            0 -> return tape'
            _ -> runLoop body tape'
