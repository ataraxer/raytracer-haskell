module Intersection
( Intersection(..)
) where

import Shape

data Intersection = Intersection Shape Double
    deriving (Eq)
