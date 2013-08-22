{-# LANGUAGE BangPatterns #-}

import Data.Char (chr, ord)
import System.IO (hFlush, stdout)

-- | Infinite list type
data Stream a = a :| Stream a

-- | Pointed container type. c is supposed to be a list (for the source) or a
--   stream (for the data tape).
data Tape c a = Tape (c a) a (c a)

-- | Move focus on stream tape right
focusRightS :: Tape Stream a -> Tape Stream a
focusRightS (Tape ls p (r :| rs)) = Tape (p :| ls) r rs

-- | Move focus on stream tape right
focusLeftS :: Tape Stream a -> Tape Stream a
focusLeftS (Tape (l :| ls) p rs) = Tape ls l (p :| rs)

-- | Move focus on list tape right
focusRightL :: Tape [] a -> Tape [] a
focusRightL (Tape ls p (r : rs)) = Tape (p : ls) r rs

-- | Move focus on list tape right
focusLeftL :: Tape [] a -> Tape [] a
focusLeftL (Tape (l : ls) p rs) = Tape ls l (p : rs)

emptyTape :: Tape Stream Int
emptyTape = Tape zeros 0 zeros
      where zeros = 0 :| zeros

iterateS :: (a -> a) -> a -> Stream a
iterateS f x = let fx = f x
               in  fx :| iterateS f fx

flush :: IO ()
flush = hFlush stdout


--class Functor w => Comonad w where

--      duplicate :: w a -> w (w a)
--      duplicate = extend id

--      extract :: w a -> a

--      extend :: (w a -> b) -> w a -> w b
--      extend f = fmap f . duplicate

--(=>>) :: (Comonad w) => w a -> (w a -> b) -> w b
--w =>> f = extend f w

--instance Functor Stream where
--      fmap f (x :| xs) = f x :| fmap f xs

--instance Functor Tape where
--      fmap f (Tape l p r) = Tape (fmap f l) (f p) (fmap f r)

--instance Comonad Tape where

--      extract (Tape _ p _) = p

--      duplicate tape@(Tape l p r) = Tape (iterateS focusLeft tape)
--                                         tape
--                                         (iterateS focusRight tape)


-- | The fundamental Brainfuck type.
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
      show (Comment c) = show c

type Brainfuck = Tape [] BrainfuckCommand

instance Show Brainfuck where
      show source@(Tape (_:_) _ _) = show (focusLeftL source)
      show (Tape _ p rs) = concatMap show (p:rs)

parseBrainfuck :: String -> Brainfuck
parseBrainfuck source = Tape [] c cs
      where (c:cs)   = map toBF source
            -- TODO: Handle empty input
            toBF '>' = GoRight
            toBF '<' = GoLeft
            toBF '+' = Increment
            toBF '-' = Decrement
            toBF '.' = Print
            toBF ',' = Read
            toBF '[' = LoopL
            toBF ']' = LoopR
            toBF  c  = Comment c

-- TODO: Function to check parenthesis balance
-- TODO: Optimizer
--        o)  [] --> warning: potential infinite loop
--        o)  < and > cancel
--        o)  + and - cancel
--        o)  Multiple comments into a single "Comment" cell
--        o)  Remove comments for execution if desired


runBrainfuck :: Brainfuck -> IO ()
runBrainfuck = run emptyTape
      where
            -- Runs a single instruction (without advancing the pointer, which
            -- is done by a subsequent call to 'step')
            run tape@(Tape l !p r) source@(Tape _ cmd _) = case cmd of

                  -- Move data pointer
                  GoRight -> step (focusRightS tape) source
                  GoLeft  -> step (focusLeftS  tape) source

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

                  -- Comment or loop with terminating condition met
                  _ -> step tape source

            -- Advances the instruction pointer
            step _    (Tape _ _ []) = return ()
            step tape source = run tape (focusRightL source)

            -- Moves the instruction pointer left until a "[" is found.
            -- The first parameter ("b" for balance) retains the current bracket
            -- balance to find the matching partner.
            seekLoopR 1 tape source@(Tape _ LoopR _) = step tape source
            seekLoopR b tape source@(Tape _ LoopR _) = seekLoopR (b-1) tape (focusRightL source)
            seekLoopR b tape source@(Tape _ LoopL _) = seekLoopR (b+1) tape (focusRightL source)
            seekLoopR b tape source = seekLoopR b tape (focusRightL source)

            -- Like seekLoopR, but in the other direction.
            seekLoopL 1 tape source@(Tape _ LoopL _) = step tape source
            seekLoopL b tape source@(Tape _ LoopR _) = seekLoopL (b+1) tape (focusLeftL source)
            seekLoopL b tape source@(Tape _ LoopL _) = seekLoopL (b-1) tape (focusLeftL source)
            seekLoopL b tape source = seekLoopL b tape (focusLeftL source)

