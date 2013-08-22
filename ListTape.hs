module ListTape (
        focusLeft
      , focusRight
) where

import Comonad
import Tape

-- | Move focus on list tape right
focusRight :: Tape [] a -> Tape [] a
focusRight (Tape ls p (r : rs)) = Tape (p : ls) r rs

-- | Move focus on list tape right
focusLeft :: Tape [] a -> Tape [] a
focusLeft (Tape (l : ls) p rs) = Tape ls l (p : rs)


instance Comonad (Tape []) where

      extract (Tape _ p _) = p

      duplicate tape@(Tape l p r) = Tape (tail $ iterate focusLeft tape)
                                         tape
                                         (tail $ iterate focusRight tape)