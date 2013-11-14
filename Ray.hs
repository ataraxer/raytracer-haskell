module Ray
( Ray(Ray)
) where

import Linal.Vector

data Ray = Ray { origin :: (Vector Double)
               , direction :: (Vector Double) }
