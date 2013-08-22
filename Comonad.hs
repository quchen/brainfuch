module Comonad (
        Comonad(..)
      , (=>>)
) where



class Functor w => Comonad w where

      duplicate :: w a -> w (w a)
      duplicate = extend id

      extract :: w a -> a

      extend :: (w a -> b) -> w a -> w b
      extend f = fmap f . duplicate



(=>>) :: (Comonad w) => w a -> (w a -> b) -> w b
w =>> f = extend f w



