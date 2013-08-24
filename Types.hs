module Types  (
        SuperfuckCommand(..)
      , SuperfuckSource(..)
) where

import Data.Word


-- | A higher-level representation of Brainfuck code; allows combination of
--   successive similar commands into one, and is easier to optimize.
data SuperfuckCommand = Move Int
                      | Add Int
                      | Print Word
                      | Read
                      | Loop SuperfuckSource
                      deriving (Eq)

data SuperfuckSource = SFSource [SuperfuckCommand]
      deriving (Eq)

instance Show SuperfuckSource where
      show (SFSource xs) = concatMap show xs


-- Adds the syntax "x(n)" for the command "x" appearing repeatedly, for example
-- "+(3)" = "+++".
instance Show SuperfuckCommand where
      show (Move i) = case (i `compare` 0, abs i) of
            (LT, j) -> '<' : showMulti j
            (EQ, _) -> ""
            (GT, j) -> '>' : showMulti j

      show (Add n) = case (n `compare` 0, abs n) of
            (LT, m) -> '-' : showMulti m
            (EQ, _) -> ""
            (GT, m) -> '+' : showMulti m

      show (Print 0)   = ""
      show (Print n)   = '.' : showMulti n
      show Read        = ","
      show (Loop body) = '[' : show body ++ "]"



-- | Used to abbreviate multiple occurrences of identical symbols, i.e. the
--   "(n)" syntax.
showMulti :: (Show a, Ord a, Num a) => a -> String
showMulti n | n <= 1    = ""
            | otherwise = show n