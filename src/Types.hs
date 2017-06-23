{-# LANGUAGE GADTs #-}

module Types where

import Control.Monad



-- | A higher-level representation of Brainfuck code; allows combination of
--   successive similar commands into one, and is easier to optimize.
data BrainfuckCommand
    = Move Int
    | Add Int
    | Print Word
    | Read
    | Loop BrainfuckSource
    deriving (Eq)

newtype BrainfuckSource = BFSource [BrainfuckCommand]
    deriving (Eq)

instance Show BrainfuckSource where
    show (BFSource []) = "(empty)"
    show (BFSource xs) = concatMap show xs

-- Adds the syntax "x(n)" for the command "x" appearing repeatedly, for example
-- "+3" = "+++".
instance Show BrainfuckCommand where
    show (Move  i) = showMulti i (if i < 0 then '<' else '>')
    show (Add   n) = showMulti n (if n < 0 then '-' else '+')
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
  where
    showMulti' 0 _ = ""
    showMulti' 1 c = [c]
    showMulti' k c = c : show k



data FreeM f a = Pure a | Roll (f (FreeM f a))

instance (Functor f) => Functor (FreeM f) where
    fmap = liftM

instance (Functor f) => Applicative (FreeM f) where
    pure = Pure
    (<*>) = ap

instance (Functor f) => Monad (FreeM f) where
    Pure x >>= f = f x
    Roll x >>= f = Roll ((>>= f) <$> x)

-- | A single operational instruction for a running Brainfuck program.
data Instruction a where
    PrintChar ::  Char -> a  -> Instruction a
    ReadChar  :: (Char -> a) -> Instruction a

-- | Chain of instructions.
type Program a = FreeM Instruction a

instance Functor Instruction where
    fmap f (PrintChar c k) = PrintChar c (f k)
    fmap f (ReadChar    k) = ReadChar    (f . k)

printChar :: Char -> Program ()
printChar c = Roll (PrintChar c (Pure ()))

readChar :: Program Char
readChar = Roll (ReadChar Pure)
