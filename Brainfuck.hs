{-# LANGUAGE BangPatterns #-}

module Brainfuck (
        BrainfuckCommand(..)
      , BrainfuckSource(..)
      , parseBrainfuck
      , runBrainfuck
) where

import Data.Char (chr, ord)

import qualified Stream as S
import qualified ListTape as L
import Comonad
import Utilities
import Tape






-- | The fundamental Brainfuck type. Isomorphic to Brainfuck source.
data BrainfuckCommand = GoRight
                      | GoLeft
                      | Increment
                      | Decrement
                      | Print
                      | Read
                      | LoopL
                      | LoopR
                      | Comment Char

instance Show BrainfuckCommand where
      show GoRight     = ">"
      show GoLeft      = "<"
      show Increment   = "+"
      show Decrement   = "-"
      show Print       = "."
      show Read        = ","
      show LoopL       = "["
      show LoopR       = "]"
      show (Comment c) = [c]

data BrainfuckSource = BFSource [BrainfuckCommand]

instance Show BrainfuckSource where
      show (BFSource xs) = concatMap show xs




-- | Checks whether the source is syntactically valid.
validateSyntax :: BrainfuckSource -> BrainfuckSource
validateSyntax source@(BFSource xs)
      | checkBrackets = source
      | otherwise     = error "Bracket mismatch"
      where checkBrackets = all (>= 0) scoreList && last scoreList == 0
            scoreList = scanl1 (+) $ map bracketScore xs
            bracketScore LoopL =  1
            bracketScore LoopR = -1
            bracketScore _     =  0







-- | Inverse of the Show instance
parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck = validateSyntax . BFSource . map toBF
      where toBF '>' = GoRight
            toBF '<' = GoLeft
            toBF '+' = Increment
            toBF '-' = Decrement
            toBF '.' = Print
            toBF ',' = Read
            toBF '[' = LoopL
            toBF ']' = LoopR
            toBF  c  = Comment c





-- | Executes a Brainfuck program. Should only be used to benchmark/check
--   'runSuperfuck'.
runBrainfuck :: BrainfuckSource -> IO ()
runBrainfuck = run S.emptyTape . bf2tape
      where
            bf2tape (BFSource []    ) = Tape [] (Comment ' ') []
            bf2tape (BFSource (b:bs)) = Tape [] b bs

            -- Runs a single instruction (without advancing the pointer, which
            -- is done by a subsequent call to 'step')
            run tape@(Tape l !p r) source = case extract source of

                  -- Move data pointer
                  GoRight -> step (S.focusRight tape) source
                  GoLeft  -> step (S.focusLeft  tape) source

                  -- Modify data
                  Increment -> step (Tape l (p+1) r) source
                  Decrement -> step (Tape l (p-1) r) source

                  -- I/O
                  Print -> putChar (chr p) >> flush >> step tape source
                  Read  -> do c <- getChar
                              step (Tape l (ord c) r) source

                  -- Loop
                  LoopL | p == 0 -> seekLoopR 0 tape source
                  LoopR | p /= 0 -> seekLoopL 0 tape source

                  -- Comment or loop with terminating condition met: do nothing
                  _ -> step tape source

            -- Advances the instruction pointer
            step _ (Tape _ _ []) = return ()
            step tape source = run tape (L.focusRight source)

            -- Moves the instruction pointer left until a "[" is found.
            -- The first parameter ("b" for balance) retains the current bracket
            -- balance to find the matching partner.
            seekLoopR 1 tape source@(Tape _ LoopR _) = step tape source
            seekLoopR b tape source =
                  let !b' = case extract source of
                                  LoopR -> b-1
                                  LoopL -> b+1
                                  _     -> b
                  in  seekLoopR b' tape (L.focusRight source)

            -- Like seekLoopR, but in the other direction.
            seekLoopL 1 tape source@(Tape _ LoopL _) = step tape source
            seekLoopL b tape source =
                  let !b' = case extract source of
                                  LoopR -> b+1
                                  LoopL -> b-1
                                  _     -> b
                  in  seekLoopL b' tape (L.focusLeft source)

