module Camera
( camera
, position
, lookAt
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
		right_ = normalize $ cross (Vec3 0 1 0) direction_
		down_ = cross right_ direction_
