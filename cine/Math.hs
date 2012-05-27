{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math where

import Graphics.Rendering.OpenGL (GLfloat, GLmatrix)

type Vector a = [a]
data Matrix a = M [a] deriving Show
data Quaternion a = Q a a a a deriving Show

addVec :: Floating a => Vector a -> Vector a -> Vector a
addVec = zipWith (+)

subVec :: Floating a => Vector a -> Vector a -> Vector a
subVec = zipWith (-)

innerVec :: Floating a => Vector a -> Vector a -> Vector a
innerVec = zipWith (*)

negateVec :: Floating a => Vector a -> Vector a
negateVec = map negate

dotVec :: Floating a => Vector a -> Vector a -> a
dotVec a b = sum $ innerVec a b

lengthVec :: Floating a => Vector a -> a
lengthVec a = sqrt . sum $ map square a
    where square x = x*x

degToRad a = a * pi / 180.0
radToDeg a = a * 180.0 / pi

lerp :: Floating a => a -> Vector a -> Vector a -> Vector a
lerp x a b = addVec a' b'
    where a' = innerVec a $ replicate 4 (1-x)
          b' = innerVec b $ replicate 4 x


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

