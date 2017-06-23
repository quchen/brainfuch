module Tape (Tape (..)) where

-- | Pointed container type. c is supposed to be a list (for the source) or a
--   stream (for the data tape).
data Tape c a = Tape (c a) a (c a)

instance (Functor f) => Functor (Tape f) where
    fmap f (Tape l p r) = Tape (fmap f l) (f p) (fmap f r)
