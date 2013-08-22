{-# LANGUAGE FlexibleInstances #-}

module Stream (
      Stream(..)
      , focusLeft
      , focusRight
      , emptyTape
) where

import Comonad
import Tape


-- | Infinite list type
data Stream a = a :| Stream a



instance Functor Stream where
      fmap f (x :| xs) = f x :| fmap f xs



-- Not needed by the program, but why not :-)
instance Comonad Stream where
      extract     (x :|  _) = x
      duplicate s@(_ :| xs) =   s :| duplicate xs
      extend f  s@(_ :| xs) = f s :| extend f  xs



instance Comonad (Tape Stream) where
      extract (Tape _ p _) = p
      duplicate tape = Tape (iterateS focusLeft tape)
                            tape
                            (iterateS focusRight tape)



-- | Like Data.List.iterate for Stream, but with the difference that "f applied
--   zero times" is not considered.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = let fx = f x
               in  fx :| iterateS f fx



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
