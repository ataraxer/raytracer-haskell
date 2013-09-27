module Linal.Vector
( Vector(Vec3)
, cross
, dot
, vdiff
, normalize
) where

import Control.Applicative

data Vector a = Vec3 a a a
	deriving (Show, Eq)

instance Functor Vector where
	fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)


fromList :: [a] -> Vector a
fromList [x, y, z] = Vec3 x y z

coords (Vec3 x y z) = [x, y, z]
square = (**2)

{-magnitude :: (Num a) => Vector a -> Double-}
magnitude = sqrt . sum . coords . fmap square

{-normalize :: (Num a) => Vector a -> Vector a-}
normalize v = fmap (/m) v
	where m = magnitude v

relate f a b = getZipList $ f <$> list a <*> list b
	where list = ZipList . coords

dot a b = sum $ relate (*) a b

cross (Vec3 ax ay az) (Vec3 bx by bz) =
	Vec3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

vdiff a b = fromList $ relate (-) a b
