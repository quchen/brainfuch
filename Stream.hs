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



instance Comonad (Tape Stream) where

      extract (Tape _ p _) = p

      duplicate tape@(Tape l p r) = Tape (iterateS focusLeft tape)
                                         tape
                                         (iterateS focusRight tape)


iterateS :: (a -> a) -> a -> Stream a
iterateS f x = let fx = f x
               in  fx :| iterateS f fx



-- | Move focus on stream tape right
focusRight :: Tape Stream a -> Tape Stream a
focusRight (Tape ls p (r :| rs)) = Tape (p :| ls) r rs



-- | Move focus on stream tape right
focusLeft :: Tape Stream a -> Tape Stream a
focusLeft (Tape (l :| ls) p rs) = Tape ls l (p :| rs)

emptyTape :: Tape Stream Int
emptyTape = Tape zeros 0 zeros
      where zeros = 0 :| zeros
