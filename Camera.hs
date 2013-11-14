module Camera
( camera
, position
, lookAt
, shiftCamera
) where

import Linal.Vector

data Camera = Camera { position  :: (Vector Double)
                     , lookAt    :: (Vector Double)
                     , right     :: (Vector Double)
                     , down      :: (Vector Double)
                     , direction :: (Vector Double)}

camera position_ lookAt_ = Camera position_ lookAt_ right_ down_ direction_
    where
        direction_ = normalize $ vdiff lookAt_ position_
        right_     = normalize $ (Vec3 0.0 1.0 0.0) >#< direction_
        down_      = cross right_ direction_

shiftCamera c x y = normalize (dir `vadd` xshift `vadd` yshift)
    where
        dir = direction c
        xshift = fmap (*(x - 0.5)) (right c)
        yshift = fmap (*(y - 0.5)) (down c)
