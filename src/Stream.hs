module Stream (
      Stream(..)
      , focusLeft
      , focusRight
      , emptyTape
) where



import Tape


-- | Infinite list type
data Stream a = a :| Stream a



-- | Move focus on stream tape right
focusRight :: Tape Stream a -> Tape Stream a
focusRight (Tape ls p (r :| rs)) = Tape (p :| ls) r rs



-- | Move focus on stream tape left
focusLeft :: Tape Stream a -> Tape Stream a
focusLeft (Tape (l :| ls) p rs) = Tape ls l (p :| rs)


-- | Tape filled with zeros
emptyTape :: Tape Stream Int
emptyTape = Tape zeros 0 zeros
      where zeros = 0 :| zeros
