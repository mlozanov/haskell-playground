{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math where

import Data.List
import Graphics.Rendering.OpenGL (GLfloat, GLmatrix)

type Vector a = [a]
data Matrix a = M [a] deriving Show
data Quaternion a = Q a a a a deriving (Eq,Ord,Show)

addVec :: Floating a => Vector a -> Vector a -> Vector a
addVec = zipWith (+)

subVec :: Floating a => Vector a -> Vector a -> Vector a
subVec = zipWith (-)

mulVec :: Floating a => Vector a -> Vector a -> Vector a
mulVec = zipWith (*)

negateVec :: Floating a => Vector a -> Vector a
negateVec = map negate

dotVec :: Floating a => Vector a -> Vector a -> a
dotVec a b = sum $ mulVec a b

lengthVec :: Floating a => Vector a -> a
lengthVec a = sqrt . sum $ map square a
    where square x = x*x

zeroV :: Floating a => Vector a
zeroV = [0,0,0,0]

x :: Floating a => Vector a -> a
y :: Floating a => Vector a -> a
z :: Floating a => Vector a -> a
w :: Floating a => Vector a -> a
x [x,_,_,_] = x
y [_,y,_,_] = y
z [_,_,z,_] = z
w [_,_,_,w] = w

degToRad a = a * pi / 180.0
radToDeg a = a * 180.0 / pi

lerp :: Floating a => a -> Vector a -> Vector a -> Vector a
lerp x a b = addVec a' b'
    where a' = mulVec a $ replicate 4 (1-x)
          b' = mulVec b $ replicate 4 x

transposeM :: Floating a => Matrix a -> Matrix a
transposeM = fromList . concat . transpose . unwords4 . toList

mulMV :: Floating a => Vector a -> Matrix a ->  Vector a
mulMV v (M ms) = map (dotVec v) $ f ms
    where f = transpose . unwords4

mulMM :: Floating a => Matrix a -> Matrix a -> Matrix a
mulMM (M as) (M bs) = M (concat $ map (\row -> map (dotVec row) a') b')
    where a' = unwords4 as
          b' = unwords4 bs

inverseM :: Floating a => Matrix a -> Matrix a
inverseM (M [m0,m1,m2,m3
            ,m4,m5,m6,m7
            ,m8,m9,m10,m11
            ,m12,m13,m14,m15]) = M ms'
    where ms' = []

unwords4 [] = []
unwords4 ys = r:(unwords4 rs)
    where (r,rs) = splitAt 4 ys

addQ :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
addQ (Q q a b c) (Q w x y z) = Q (q+w) (a+x) (b+y) (c+z)

subQ :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
subQ (Q q a b c) (Q w x y z) = Q (q-w) (a-x) (b-y) (c-z)

mulQ :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
mulQ (Q a b c d) (Q w x y z) = Q (a*w - b*x - c*y - d*z)
                                 (a*x + b*w + c*z - d*y)
                                 (a*y - b*z + c*w + d*x)
                                 (a*z + b*y - c*x + d*w)

conjQ :: Floating a => Quaternion a -> Quaternion a
conjQ (Q a b c d) = Q a (-b) (-c) (-d)

magQ :: Floating a => Quaternion a -> a
magQ (Q a b c d) = sqrt (a*a + b*b + c*c + d*d)

normQ :: (Floating a, Ord a) => Quaternion a -> Quaternion a
normQ (Q w x y z) | mag < 0.0001 = Q w x y z
                  | otherwise = Q w' x' y' z'
    where mag = magQ (Q w x y z)
          w' = w / mag
          x' = x / mag 
          y' = y / mag
          z' = z / mag

identityQ :: Floating a => Quaternion a
identityQ = Q 1 0 0 0

toMatrixQ :: Floating a => Quaternion a -> Matrix a
toMatrixQ (Q w x y z) = M [ 1.0 - 2*y*y - 2*z*z, 2*x*y - 2*z*w, 2*x*z + 2*y*w, 0.0
                          , 2*x*y + 2*z*w, 1.0 - 2*x*x - 2*z*z, 2*y*z + 2*x*w, 0.0
                          , 2*x*z - 2*y*w, 2*y*z - 2*x*w, 1.0 - 2*x*x - 2*y*y, 0.0
                          , 0.0, 0.0, 0.0, 1.0 ]

fromMatrixQ [ a,b,c,d
            , e,f,g,h
            , i,j,k,l
            , m,n,o,p ] = Q w x y z
    where w = sqrt (1.0 + a + f + k) / 2.0
          x = (j - g) / (4.0 * w)
          y = (i - c) / (4.0 * w)
          z = (e - b) / (4.0 * w)

fromAxisAngleQ :: Floating a => a -> a -> a -> a -> Quaternion a
fromAxisAngleQ x y z angle = Q ca (x*sa) (y*sa) (z*sa)
    where ca = cos (angle * 0.5)
          sa = sin (angle * 0.5)

toAxisAngleQ :: (Floating a, Ord a) => Quaternion a -> ([a], a)
toAxisAngleQ (Q w x y z) | abs w > 0.9999 = ([0,0,1.0], 0.0)
                         | otherwise = ([ax,ay,az], angle)
    where ww = 1.0 - w*w
          angle = 2*acos w
          ax = x / sqrt ww
          ay = y / sqrt ww
          az = z / sqrt ww

fromList :: Floating a => [a] -> Matrix a
fromList l = M l

toList :: Floating a => Matrix a -> [a]
toList (M ms) = ms

identity :: Floating a => Matrix a
identity = M [1,0,0,0
             ,0,1,0,0
             ,0,0,1,0
             ,0,0,0,1]

frustum :: Floating a => a -> a -> a -> a -> a -> a -> Matrix a
frustum left right top bottom near far = M [ x, 0, a, 0
                                           , 0, y, b, 0
                                           , 0, 0, c, d
                                           , 0, 0, -1, 0 ]
    where x = 2*near/(right-left)
          y = 2*near/(top-bottom)
          a = (right+left)/(right-left)
          b = (top+bottom)/(top-bottom)
          c = -(far+near)/(far-near)
          d = -2*far*near/(far-near)

perspective :: Floating a => Matrix a
perspective = frustum left right top bottom near far
    where near = 1.0
          far = 2000.0
          left = top * aspect
          right = bottom * aspect
          top = near * tan (60.0 * pi / 360.0)
          bottom = -top
          aspect = 1.78

-- not correct yet
lookat :: Floating a => Vector a -> Vector a -> Vector a -> Vector a -> Matrix a
lookat up side forward position = M (concat [up,side,forward,position])

translate :: Floating a => a -> a -> a -> Matrix a
translate x y z = M [1,0,0,x
                    ,0,1,0,y
                    ,0,0,1,z
                    ,0,0,0,1]

rotate :: Floating a => a -> a -> a -> Matrix a
rotate ax ay az = identity

--- debug

t :: Floating a => Matrix a -> [[a]]
t (M xs) = unwords4 xs

t' = t (M [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15.0])
l' = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15.0]
m' = M [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15.0]

q' = normQ $ fromAxisAngleQ 0.707 0.707 0 (degToRad 45.0)
q'' = normQ $ fromAxisAngleQ 1.0 0.0 0 (degToRad 15.0)

a' = mulMM a (transposeM a)
    where a = toMatrixQ q'

a'' = mulMM a (transposeM a)
    where a = toMatrixQ q''
