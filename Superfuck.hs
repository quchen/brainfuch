{-# LANGUAGE BangPatterns #-}

module Superfuck (
        bf2sf
      , sf2bf
      , optimize
      , sf2tape
      , runSuperfuck
) where

import Data.Char (chr, ord)
import System.IO (hFlush, stdout)
import Data.Word
import Data.Maybe (mapMaybe)
import Data.List
import Data.Monoid
import Debug.Trace

import Stream (Stream(..))
import qualified Stream as S
import qualified ListTape as L
import Comonad
import Tape
import Utilities
import Brainfuck



type SuperfuckTape = Tape [] SuperfuckCommand



-- A higher-level representation of Brainfuck code; combines successive similar
-- commands into one, and is easier to optimize.
data SuperfuckCommand = Go Int
                      | Add Int
                      | Print' Word
                      | Read'
                      | LoopL'
                      | LoopR'
                    | Comment'   String
                      deriving (Eq)

instance Show SuperfuckCommand where
      show (Go i) = case (i `compare` 0, abs i) of
            (LT, 1)  -> "<"
            (LT, i') -> "<(" ++ show i' ++ ")"
            (EQ, _)  -> ""
            (GT, 1)  -> ">"
            (GT, i') -> ">(" ++ show i' ++ ")"

      show (Add n) = case (n `compare` 0, abs n) of
            (LT, 1)  -> "-"
            (LT, n') -> "-(" ++ show n' ++ ")"
            (EQ, _)  -> ""
            (GT, 1)  -> "+"
            (GT, n') -> "+(" ++ show n' ++ ")"

      show (Print' 0)   = ""
      show (Print' 1)   = "."
      show (Print' n)   = ".(" ++ show n ++ ")"
      show Read'        = show Read
      show LoopL'       = show LoopL
      show LoopR'       = show LoopR
      show (Comment' s) = s

data SuperfuckSource = SFSource [SuperfuckCommand]
      deriving (Eq)

instance Show SuperfuckSource where
      show (SFSource xs) = concatMap show xs

-- | Brainfuck to Superfuck conversion. Inverse of sf2bf.
bf2sf :: BrainfuckSource -> SuperfuckSource
bf2sf (BFSource xs) = SFSource $ map convert xs
      where convert GoRight = Go 1
            convert GoLeft  = Go (-1)
            convert Increment = Add 1
            convert Decrement = Add (-1)
            convert Print = Print' 1
            convert Read  = Read'
            convert LoopL = LoopL'
            convert LoopR = LoopR'
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


-- One optimization pass.
-- TODO: Make optimizations dependent on parameters
optimizePass :: SuperfuckSource -> SuperfuckSource
optimizePass (SFSource xs) = SFSource $ mapMaybe dropRedundant .
                                        concatMap combine .
                                        groupBy equivalence $
                                        xs
      where equivalence (Go       _) (Go       _) = True
            equivalence (Add      _) (Add      _) = True
            equivalence (Print'   _) (Print'   _) = True
            equivalence (Comment' _) (Comment' _) = True
            equivalence _ _ = False

            combine go@(Go _:_) = [combineGo go]
            combine add@(Add _:_) = [combineAdd add]
            combine print'@(Print' _:_) = [combinePrint' print']
            combine comment'@(Comment' _:_) = [combineComment' comment']
            combine xs = xs

            -- TODO: Monoid instances for new Go/Add/Comment types? All four
            --       functions below would then just be mconcat.

            -- < and > add up/cancel
            combineGo = Go . sum . map (\(Go i) -> i)

            -- + and - add up/cancel
            combineAdd = Add . sum . map (\(Add n) -> n)

            -- Printing the same character multiple times
            combinePrint' = Print' . sum . map (\(Print' i) -> i)

            -- Comment "a", Comment "b" ==> Comment "ab"
            combineComment' = Comment' . concatMap (\(Comment' x) -> x)

            dropRedundant (Comment' _) = Nothing
            dropRedundant (Add 0) = Nothing
            dropRedundant (Go 0) = Nothing
            dropRedundant x = Just x

            -- TODO: [] --> warning: potential infinite loop

optimize :: SuperfuckSource -> SuperfuckSource
optimize sfSource = let opt = optimizePass sfSource
                    in  if opt == sfSource then sfSource
                        else optimize opt



sf2tape :: SuperfuckSource -> SuperfuckTape
sf2tape (SFSource (b:bs)) = Tape [] b bs



-- | Executes a Superfuck program
runSuperfuck :: SuperfuckTape -> IO ()
runSuperfuck = run S.emptyTape
      where
            -- Apply f n times
            times n f = appEndo . mconcat . map Endo $ replicate n f

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
                  Read' -> do c <- getChar
                              step (Tape l (ord c) r) source

                  -- Loop
                  LoopL' | p == 0 -> seekLoopR 0 tape source
                  LoopR' | p /= 0 -> seekLoopL 0 tape source

                  -- Comment or loop with terminating condition met: do nothing
                  _ -> step tape source

            step _ (Tape _ _ []) = return ()
            step tape source = run tape (L.focusRight source)

            -- Moves the instruction pointer left until a "[" is found.
            -- The first parameter ("b" for balance) retains the current bracket
            -- balance to find the matching partner.
            seekLoopR 1 tape source@(Tape _ LoopR' _) = step tape source
            seekLoopR b tape source =
                  let !b' = case extract source of
                                  LoopR' -> b-1
                                  LoopL' -> b+1
                                  _      -> b
                  in  seekLoopR b' tape (L.focusRight source)

            -- Like seekLoopR, but in the other direction.
            seekLoopL 1 tape source@(Tape _ LoopL' _) = step tape source
            seekLoopL b tape source =
                  let !b' = case extract source of
                                  LoopR' -> b+1
                                  LoopL' -> b-1
                                  _      -> b
                  in  seekLoopL b' tape (L.focusLeft source)