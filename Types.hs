module Types  (
        BrainfuckCommand(..)
      , BrainfuckSource(..)
) where

import Data.Word


-- | A higher-level representation of Brainfuck code; allows combination of
--   successive similar commands into one, and is easier to optimize.
data BrainfuckCommand = Move Int
                      | Add Int
                      | Print Word
                      | Read
                      | Loop BrainfuckSource

data BrainfuckSource = SFSource [BrainfuckCommand]

instance Show BrainfuckSource where
      show (SFSource xs) = concatMap show xs


-- Adds the syntax "x(n)" for the command "x" appearing repeatedly, for example
-- "+3" = "+++".
instance Show BrainfuckCommand where
      show (Move  i) = showMulti i $ if i < 0 then '<' else '>'
      show (Add   n) = showMulti n $ if n < 0 then '-' else '+'
      show (Print 0) = ""
      show (Print n) = showMulti n '.'
      show  Read     = ","
      show (Loop  b) = '[' : show b ++ "]"



-- | Used to abbreviate multiple occurrences of identical symbols, i.e. the
--   "(n)" syntax.
showMulti :: (Show a, Ord a, Num a)
          => a      -- Int/Word for how many times the character appears
          -> Char   -- ^ Character to show
          -> String
showMulti n = showMulti' (abs n)
      where showMulti' 0 _ = ""
            showMulti' 1 c = [c]
            showMulti' k c = c : show k
