{-# LANGUAGE BangPatterns #-}

module Compile (
      compileBF
) where

import Control.Monad
import Data.Char     (chr, ord)
import Data.Monoid

import qualified Stream as S
import           Tape
import           Types



-- | Executes a Brainfuck program (as given; if optimizations are desired apply
--   them explicitly).
compileBF :: BrainfuckSource -> Program ()
compileBF = void . flip compile S.emptyTape



-- | Apply an endomorphism multiple times
times :: Int
      -> (a -> a)
      -> (a -> a)
times n f = appEndo . mconcat . map Endo $ replicate n f



-- | Compile the 'BrainfuckSource' to a free-monad-based program, ready to be
--   executed by a runtime system.
compile :: BrainfuckSource             -- ^ Instruction tape
        -> Tape S.Stream Int           -- ^ Data tape
        -> Program (Tape S.Stream Int) -- ^ Tape after termination
compile (BFSource []    ) tape               = return tape
compile (BFSource (x:xs)) tape@(Tape l !p r) = let rest = BFSource xs
                                               in  case x of

      Move n -> compile rest (abs n `times` f $ tape)
            where f | n < 0     = S.focusLeft
                    | otherwise = S.focusRight

      Add n -> compile rest (Tape l (p+n) r)

      Print n -> do replicateM_ (fromIntegral n) (printChar (chr $ p `mod` 2^(8 :: Int)))
                    compile rest tape

      Read -> do c <- readChar
                 compile rest (Tape l (ord c) r)

      Loop body | p == 0    -> compile rest tape  -- ignore entire loop
                | otherwise -> compileLoop body tape >>= compile rest



compileLoop :: BrainfuckSource             -- ^ Loop body
            -> Tape S.Stream Int           -- ^ Data tape
            -> Program (Tape S.Stream Int) -- ^ Tape after the loop terminates
compileLoop body tape = do
      tape'@(Tape _ p _) <- compile body tape
      case p of
            0 -> return tape'
            _ -> compileLoop body tape'
