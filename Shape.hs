module Shape
( Shape(..)
, shapeNormal
) where

import Linal.Vector
import Ray

type Color = (Double, Double, Double)
data Shape = Plain  (Vector Double) Double Color
           | Sphere (Vector Double) Double Color
           deriving (Eq)

shapeNormal (Plain n _ _)  = n
shapeNormal (Sphere n _ _) = n


normalAt :: Shape -> Vector Double -> Vector Double
normalAt (Plain  normal _ _) _ = normal
normalAt (Sphere center _ _) p = normalize (p `vdiff` center)

intersectWith s@(Plain normal distance _) (Ray origin direction)
    | a > 0     = Nothing
    | otherwise = Just (s, d)
    where a = direction >.< normal
          b = normal >.< (origin ->- (normal $>* distance))
          d = (-b) / a :: Double

intersectWith s@(Sphere center radius _) (Ray origin direction)
    | result /= Nothing = Just (s, unjust result)
    | otherwise = Nothing
    where a = 1
          b =  sum $ coords $ ((origin ->- center) ->* direction) $>* 2
          c = (sum $ coords $  (origin ->- center) $>** 2) - (radius ** 2)
          roots = solve a b c
          pickroot (Nothing, Nothing) = Nothing
          pickroot (Just r, Nothing)  = Just r
          pickroot (Just r1, Just r2) = Just (min r1 r2)
          result = pickroot roots
          unjust (Just x) = x

solve a b c
    | d > 0 = (Just r1, Just r2)
    | otherwise = (Nothing, Nothing)
    where d = (b * b) - (4 * a * c)
          r1 = (-b) - (sqrt d) / 2 - 0.000001
          r2 = (-b) + (sqrt d) / 2 - 0.000001
