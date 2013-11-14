module Linal.Vector
where

import Control.Applicative

data Vector a = Vec3 a a a
    deriving (Show, Eq)

instance Functor Vector where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)


fromList :: [Double] -> Vector Double
fromList [x, y, z] = Vec3 (x :: Double) (y :: Double) (z :: Double)

coords (Vec3 x y z) = [x, y, z]
square = (**2)

magnitude = sqrt . sum . coords . fmap square

normalize v = fmap (/m) v
    where m = magnitude v

relate f a b = zipWith f (coords a) (coords b)

dot a b = sum $ relate (*) a b

cross (Vec3 ax ay az) (Vec3 bx by bz) =
    Vec3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

vop f a b = fromList $ relate f a b

a ->+ b = vop (+) a b
a ->- b = vop (-) a b
a ->* b = vop (*) a b
a ->/ b = vop (/) a b

v $>+ s = fmap (+s) v
v $>- s = fmap (-s) v
v $>* s = fmap (*s) v
v $>/ s = fmap (/s) v
v $>** s = fmap (**s) v

a >.< b = dot a b
a >#< b = cross a b

vadd  = vop (+)
vdiff = vop (-)
vmult = vop (*)
vdiv  = vop (/)
