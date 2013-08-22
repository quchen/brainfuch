{-# LANGUAGE BangPatterns #-}

import Data.Char (chr, ord)
import System.IO (hFlush, stdout)
import Data.Word
import Data.Maybe (mapMaybe)
import Data.List

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
      show (Comment c) = [c]

type BrainfuckTape = Tape [] BrainfuckCommand
data BrainfuckSource = BFSource [BrainfuckCommand]

instance Show BrainfuckSource where
      show (BFSource xs) = concatMap show xs

instance Show BrainfuckTape where
      show source@(Tape (_:_) _ _) = show (focusLeftL source)
      show (Tape _ p rs) = concatMap show (p:rs)

-- A higher-level representation of Brainfuck code; combines successive similar
-- commands into one, and is easier to optimize.
data SuperfuckCommand = Go Int
                      | Add Int
                      | Print' Word
                      | Read'
                      | LoopL'
                      | LoopR'
                      | Comment' String
                      deriving (Eq)

instance Show SuperfuckCommand where
      show (Go i) = case i `compare` 0 of
            LT -> concat . replicate (abs i) $ show GoLeft
            EQ -> ""
            GT -> concat . replicate i $ show GoRight

      show (Add n) = case n `compare` 0 of
            LT -> concat . replicate (abs n) $ show Decrement
            EQ -> ""
            GT -> concat . replicate n $ show Increment

      show (Print' 0)   = ""
      show (Print' n)   = concat . replicate (fromIntegral n) $ show Print
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



-- TODO: Function to check parenthesis balance

-- One optimization pass.
-- TODO: Make optimizations dependent on parameters
optimizePass :: SuperfuckSource -> SuperfuckSource
optimizePass (SFSource xs) = SFSource $ mapMaybe dropRedundant .
                                        concatMap combine .
                                        groupBy equivalence $
                                        xs
      where equivalence (Go     _)   (Go     _)   = True
            equivalence (Add    _)   (Add    _)   = True
            equivalence (Print' _)   (Print' _)   = True
            equivalence (Comment' _) (Comment' _) = True
            equivalence _ _ = False

            combine go@(Go _:_) = [combineGo go]
            combine add@(Add _:_) = [combineAdd add]
            combine print'@(Print' _:_) = [Print' . fromIntegral $ length print']
            combine comment'@(Comment' _:_) = [combineComment' comment']
            combine xs = xs

            -- < and > add up/cancel
            combineGo = Go . sum . map (\(Go i) -> i)

            -- + and - add up/cancel
            combineAdd = Add . sum . map (\(Add n) -> n)

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





-- | Should be the inverse to the Show function
parseBrainfuck :: String -> BrainfuckSource
parseBrainfuck source = BFSource $ map toBF source
      where toBF '>' = GoRight
            toBF '<' = GoLeft
            toBF '+' = Increment
            toBF '-' = Decrement
            toBF '.' = Print
            toBF ',' = Read
            toBF '[' = LoopL
            toBF ']' = LoopR
            toBF  c  = Comment c

bf2tape :: BrainfuckSource -> BrainfuckTape
bf2tape (BFSource (b:bs)) = Tape [] b bs


runBrainfuck :: BrainfuckTape -> IO ()
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
            seekLoopR !b tape source@(Tape _ cmd _) =
                  let b' = case cmd of LoopR -> b-1
                                       LoopL -> b+1
                                       _     -> b
                  in  seekLoopR b' tape (focusRightL source)

            -- Like seekLoopR, but in the other direction.
            seekLoopL 1 tape source@(Tape _ LoopL _) = step tape source
            seekLoopL !b tape source@(Tape _ cmd _) =
                  let b' = case cmd of LoopR -> b+1
                                       LoopL -> b-1
                                       _     -> b
                  in  seekLoopL b' tape (focusLeftL source)

