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
        bf2sf
      , sf2bf
      , optimize
      , runSuperfuck
) where

import Data.Char (chr, ord)
import Data.Word
import Data.Maybe (mapMaybe)
import Data.List
import Data.Monoid

import qualified Stream as S
import qualified ListTape as L
import Comonad
import Tape
import Utilities
import Brainfuck




-- | A higher-level representation of Brainfuck code; allows combination of
--   successive similar commands into one, and is easier to optimize.
data SuperfuckCommand = Go Int
                      | Add Int
                      | Print' Word
                      | Read'
                      | LoopL'
                      | LoopR'
                      | Comment' String
                      deriving (Eq)

-- Adds the syntax "x(n)" for the command "x" appearing repeatedly, for example
-- "+(3)" = "+++".
instance Show SuperfuckCommand where
      show (Go i) = case (i `compare` 0, abs i) of
            (LT, j) -> show GoLeft ++ showMulti j
            (EQ, _) -> ""
            (GT, j) -> show GoRight ++ showMulti j

      show (Add n) = case (n `compare` 0, abs n) of
            (LT, m) -> show Decrement ++ showMulti m
            (EQ, _) -> ""
            (GT, m) -> show Increment ++ showMulti m

      show (Print' 0)   = ""
      show (Print' n)   = show Print ++ showMulti n
      show Read'        = show Read
      show LoopL'       = show LoopL
      show LoopR'       = show LoopR
      show (Comment' s) = s



-- | Used to abbreviate multiple occurrences of identical symbols, i.e. the
--   "(n)" syntax.
showMulti :: (Show a, Ord a, Num a) => a -> String
showMulti n | n <= 1    = ""
            | otherwise = show n



data SuperfuckSource = SFSource [SuperfuckCommand]
      deriving (Eq)

instance Show SuperfuckSource where
      show (SFSource xs) = concatMap show xs



-- | Brainfuck to Superfuck conversion. Inverse of sf2bf.
bf2sf :: BrainfuckSource -> SuperfuckSource
bf2sf (BFSource xs) = SFSource $ map convert xs
      where convert GoRight     = Go 1
            convert GoLeft      = Go (-1)
            convert Increment   = Add 1
            convert Decrement   = Add (-1)
            convert Print       = Print' 1
            convert Read        = Read'
            convert LoopL       = LoopL'
            convert LoopR       = LoopR'
            convert (Comment c) = Comment' [c]



-- | Superfuck to Brainfuck conversion. Inverse of bf2sf.
sf2bf :: SuperfuckSource -> BrainfuckSource
sf2bf (SFSource xs) = BFSource $ concatMap convert xs
      where convert (Go n) = case n `compare` 0 of
                  LT -> replicate (abs n) GoLeft
                  EQ -> []
                  GT -> replicate n GoRight

            convert (Add n) = case n `compare` 0 of
                  LT -> replicate (abs n) Decrement
                  EQ -> []
                  GT -> replicate n Increment

            convert (Print' 0) = []
            convert (Print' n) = replicate (fromIntegral n) Print
            convert Read'  = [Read]
            convert LoopL' = [LoopL]
            convert LoopR' = [LoopR]

            convert (Comment' []) = []
            convert (Comment' cs) = map Comment cs



-- | Applies one optimization pass. List of optimizations:
--
--     * Combine successive + and -, < and >, comments, print statements
--     * Remove redundant commands, e.g. comments and adding 0

-- TODO: Make optimizations dependent on parameters
optimizePass :: SuperfuckSource -> SuperfuckSource
optimizePass (SFSource xs) = SFSource .
                             mapMaybe dropRedundant .
                             concatMap combine .
                             groupBy equivalence $
                             xs



-- | Equivalence relation used to group similar code parts together
equivalence :: SuperfuckCommand -> SuperfuckCommand -> Bool
equivalence (Go       _) (Go       _) = True
equivalence (Add      _) (Add      _) = True
equivalence (Print'   _) (Print'   _) = True
equivalence (Comment' _) (Comment' _) = True
equivalence _            _            = False



-- | Combine a homogeneous list of instructions into one
combine :: [SuperfuckCommand] -> [SuperfuckCommand]
combine go@(Go _:_)             = [combineGo go]
combine add@(Add _:_)           = [combineAdd add]
combine print'@(Print' _:_)     = [combinePrint' print']
combine comment'@(Comment' _:_) = [combineComment' comment']
combine ys                      = ys

-- TODO: Monoid instances for new Go/Add/Comment types? All four
--       functions below would then just be mconcat.



-- | < and > add up/cancel
combineGo :: [SuperfuckCommand] -> SuperfuckCommand
combineGo = Go . sum . map (\(Go i) -> i)



-- | + and - add up/cancel
combineAdd :: [SuperfuckCommand] -> SuperfuckCommand
combineAdd = Add . sum . map (\(Add n) -> n)



-- | Printing the same character multiple times
combinePrint' :: [SuperfuckCommand] -> SuperfuckCommand
combinePrint' = Print' . sum . map (\(Print' i) -> i)



-- | Comment "a", Comment "b" ==> Comment "ab"
combineComment' :: [SuperfuckCommand] -> SuperfuckCommand
combineComment' = Comment' . concatMap (\(Comment' x) -> x)



-- | Used to remove NOOPs in Superfuck source
dropRedundant (Comment' _) = Nothing
dropRedundant (Add 0)      = Nothing
dropRedundant (Go 0)       = Nothing
dropRedundant x            = Just x

-- TODO: [] --> warning: potential infinite loop



-- | Applies optimizations repeatedly until the code doesn't change anymore.
optimize :: SuperfuckSource -> SuperfuckSource
optimize sfSource | optimized == sfSource = sfSource
                  | otherwise = optimize optimized
                  where optimized = optimizePass sfSource



-- | Executes a Superfuck program (as given; if optimizations are desired apply
--   them explicitly).
runSuperfuck :: SuperfuckSource -> IO ()
runSuperfuck = run S.emptyTape . sf2tape



-- | Writes Superfuck source on an instruction tape
sf2tape :: SuperfuckSource -> Tape [] SuperfuckCommand
sf2tape (SFSource []    ) = Tape [] (Comment' " ") []
sf2tape (SFSource (b:bs)) = Tape [] b bs



-- | Apply an endomorphism multiple times
times :: Int
      -> (a -> a)
      -> (a -> a)
times n f = appEndo . mconcat . map Endo $ replicate n f



-- | Execute the command at the current location of the instruction tape
run :: Tape S.Stream Int        -- ^ Data tape
    -> Tape [] SuperfuckCommand -- ^ Instruction tape
    -> IO ()
run tape@(Tape l !p r) source = case extract source of

      -- Move data pointer
      Go n -> step (abs n `times` f $ tape) source
            where f | n > 0 = S.focusRight
                    | n < 0 = S.focusLeft
                    | otherwise = error "Zero encountered, bug"

      -- Modify data
      Add n -> step (Tape l (p+n) r) source

      -- I/O
      Print' n -> do putStr (replicate (fromIntegral n) (chr p))
                     flush
                     step tape source
      Read'    -> do c <- getChar
                     step (Tape l (ord c) r) source

      -- Loop
      LoopL' | p == 0 -> seekLoopR 0 tape source
      LoopR' | p /= 0 -> seekLoopL 0 tape source

      -- Comment or loop with terminating condition met: do nothing
      _ -> step tape source



-- Advance instruction pointer (or terminate)
step :: Tape S.Stream Int        -- ^ Data tape
     -> Tape [] SuperfuckCommand -- ^ Instruction tape
     -> IO ()
step _ (Tape _ _ []) = return ()
step tape source = run tape (L.focusRight source)



-- | Moves the instruction pointer left until a "[" is found.
seekLoopR :: Int                      -- ^ Bracket balance
          -> Tape S.Stream Int        -- ^ Data tape
          -> Tape [] SuperfuckCommand -- ^ Instruction tape
          -> IO ()
seekLoopR 1 tape source@(Tape _ LoopR' _) = step tape source
      -- Note: The counter is 1, because with a counter of 1 hitting another
      --       LoopR' means the balance becomes zero, and the algorithm
      --       terminates.
seekLoopR b tape source = seekLoopR b' tape (L.focusRight source)
      where !b' = case extract source of
                        LoopR' -> b-1
                        LoopL' -> b+1
                        _      -> b



-- | Like seekLoopR, but in the other direction.
seekLoopL :: Int                      -- ^ Bracket balance
          -> Tape S.Stream Int        -- ^ Data tape
          -> Tape [] SuperfuckCommand -- ^ Instruction tape
          -> IO ()
seekLoopL 1 tape source@(Tape _ LoopL' _) = step tape source
seekLoopL b tape source = seekLoopL b' tape (L.focusLeft source)
      where !b' = case extract source of
                        LoopR' -> b+1
                        LoopL' -> b-1
                        _      -> b
