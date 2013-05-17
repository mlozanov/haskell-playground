{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Math where

import System.Random
import Data.List
import Graphics.Rendering.OpenGL (GLfloat, GLmatrix)

type Vector a = [a]
data Matrix a = M [a] deriving Show
data Quaternion a = Q a a a a deriving Show

data Shape a = Rectangle a a a a 
             | Circle (Vector a) a
             | Sphere (Vector a) a
             | Box (Vector a) (Vector a) -- center, extent
             deriving (Eq, Ord, Show)

type Spline a = [Vector a]

{-# INLINE addVec #-} 
{-# INLINE subVec #-}
{-# INLINE mulVec #-}
{-# INLINE negateVec #-}
{-# INLINE dotVec #-}
{-# INLINE lengthVec #-}
{-# INLINE mulScalarVec #-}
{-# INLINE zeroV #-} 
{-# INLINE identityV #-}

addVec :: Floating a => Vector a -> Vector a -> Vector a
addVec [x,y,z,w] [x2,y2,z2,w2] = [x+x2,y+y2,z+z2,w+w2]
addVec p q = zipWith (+) p q

subVec :: Floating a => Vector a -> Vector a -> Vector a
subVec [x,y,z,w] [x2,y2,z2,w2] = [x-x2,y-y2,z-z2,w-w2]
subVec p q = zipWith (-) p q

mulVec :: Floating a => Vector a -> Vector a -> Vector a
mulVec [x,y,z,w] [x2,y2,z2,w2] = [x*x2,y*y2,z*z2,w*w2]
mulVec p q = zipWith (*) p q

mulScalarVec :: Floating a => a -> Vector a -> Vector a
mulScalarVec s [x,y,z,w] = [s*x,s*y,s*z,s*w]
mulScalarVec s v = map (* s) v

scaleVec :: Floating a => a -> Vector a -> Vector a
scaleVec = mulScalarVec

negateVec :: Floating a => Vector a -> Vector a
negateVec = map negate

dotVec :: Floating a => Vector a -> Vector a -> a
dotVec a b = sum $ mulVec a b

lengthVec :: Floating a => Vector a -> a
lengthVec a = sqrt . sum $ map square a
    where square x = x*x

squareLengthVec :: Floating a => Vector a -> a
squareLengthVec a = sum $ map sq a
  where sq x = x*x

zeroV :: Floating a => Vector a
zeroV = [0,0,0]

identityV :: Floating a => Vector a
identityV = [0,0,0]

normalizeV :: (Floating a, Ord a) => Vector a -> Vector a
normalizeV v = if len > 0.0 then scaleVec (1.0/len) v
               else [0.0,0.0,1.0]
  where len = lengthVec v

rndVec :: IO (Vector Float)
rndVec = mapM (\_ -> randomRIO (-1.0,1.0)) [1..3]

rndVec2d :: IO (Vector Float)
rndVec2d = do
  rx <- randomRIO (-1.0,1.0)
  ry <- randomRIO (-1.0,1.0)

  return [rx,ry,0.0]

rndPolarV :: StdGen -> (Vector Float, StdGen)
rndPolarV gen = (v, nextGen)
  where (azimut, nextGen) = randomR (-pi,pi) gen 
        x = cos azimut
        y = sin azimut
        v = [x,y,0.0]

rndPolarVec :: IO (Vector Float)
rndPolarVec = do 
  azimut <- randomRIO (-pi, pi)
  let x = 1.0 * cos azimut
      y = 1.0 * sin azimut
      z = 0.0
   in return [x,y,z]

rndSphereVec :: IO (Vector Float)
rndSphereVec = do
  azimut <- randomRIO (-pi, pi)
  zenith <- randomRIO (-pi, pi)
  let x = sin zenith * cos azimut
      y = sin zenith * sin azimut
      z = cos zenith
   in return [x,y,z]

rndCylinderVec :: IO (Vector Float)
rndCylinderVec = do
  azimut <- randomRIO (-pi, pi)
  height <- randomRIO (-1.0, 1.0)
  let x = 1.0 * cos azimut
      y = 1.0 * sin azimut
   in return [x,y,height]


circleVec :: Float ->  Vector Float
circleVec i = [x,y,z]
  where x = cos i
        y = sin i
        z = 0.0

x :: Floating a => Vector a -> a
y :: Floating a => Vector a -> a
z :: Floating a => Vector a -> a
w :: Floating a => Vector a -> a
x (x:y:z:rest) = x
y (x:y:z:rest) = y
z (x:y:z:rest) = z
w (x:y:z:w:rest) = w

{-# INLINE degToRad #-}
{-# INLINE radToDeg #-}
{-# INLINE lerp #-}

degToRad a = a * pi / 180.0
radToDeg a = a * 180.0 / pi

lerp :: Floating a => a -> Vector a -> Vector a -> Vector a
lerp x a b = addVec a' b'
    where a' = mulVec a $ replicate (length a) (1-x)
          b' = mulVec b $ replicate (length b) x

slerp :: Floating a => a -> Quaternion a -> Quaternion a -> Quaternion a
slerp x q0 q1 = undefined

euler :: Floating a => a -> Vector a -> Vector a -> Vector a
euler !h !p0 !p = addVec p0 (mulScalarVec h p)

transposeM :: Floating a => Matrix a -> Matrix a
transposeM = fromList . concat . transpose . unwords4 . toList

mulMV :: Floating a => Vector a -> Matrix a ->  Vector a
mulMV v (M ms) = map (dotVec v) $ f ms
    where f = unwords4

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

{-# INLINE addQ #-}
{-# INLINE subQ #-}
{-# INLINE mulQ #-}
{-# INLINE conjQ #-}
{-# INLINE magQ #-}
{-# INLINE normQ #-}
{-# INLINE scaleQ #-}
{-# INLINE identityQ #-}
{-# INLINE toMatrixQ #-}
{-# INLINE fromMatrixQ #-} 
{-# INLINE fromAxisAngleQ #-}

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

scaleQ :: Floating a => a -> Quaternion a -> Quaternion a
scaleQ v (Q w x y z) = Q (v*w) (v*x) (v*y) (v*z)

identityQ :: Floating a => Quaternion a
identityQ = Q 1 0 0 0

toMatrixQ :: Floating a => Quaternion a -> Matrix a
toMatrixQ (Q w x y z) = M [ 1.0 - 2*y*y - 2*z*z,       2*x*y + 2*z*w,       2*x*z - 2*y*w, 0.0
                          ,       2*x*y - 2*z*w, 1.0 - 2*x*x - 2*z*z,       2*y*z - 2*x*w, 0.0
                          ,       2*x*z + 2*y*w,       2*y*z + 2*x*w, 1.0 - 2*x*x - 2*y*y, 0.0
                          , 0.0, 0.0, 0.0, 1.0 ]

fromMatrixQ :: Floating a => [a] -> Quaternion a
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
          sqww = 1.0 / sqrt ww
          angle = 2.0 * acos w
          ax = x * sqww
          ay = y * sqww
          az = z * sqww

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
          left = -top * aspect
          right = top * aspect
          top = near * tan (45.0 * pi / 360.0)
          bottom = -top
          aspect = 1.85

-- not correct yet
lookat :: Floating a => Vector a -> Vector a -> Vector a -> Vector a -> Matrix a
lookat up side forward position = M (concat [up,side,forward,position])

translate :: Floating a => a -> a -> a -> Matrix a
translate x y z = M [1,0,0,0 -- column 1
                    ,0,1,0,0 -- column 2
                    ,0,0,1,0 -- column 3 etc
                    ,x,y,z,1] -- column major 

rotate :: Floating a => a -> a -> a -> Matrix a
rotate ax ay az = identity

rotatex :: Floating a => a -> Matrix a
rotatex ax = rotate ax 0 0

rotate2d :: Floating a => a -> Vector a -> Vector a
rotate2d angle (x:y:rest) = x':y':rest
  where x' = x*cos angle - y*sin angle
        y' = y*cos angle + x*sin angle

-- curves
--linearInterpolate :: Floating a => [a] -> a -> a
linearInterpolate :: [Float] -> Float -> Float
linearInterpolate cs t = (1.0 - ((fromIntegral high) - t)) * (a-b) + b
  where high = ceiling t
        low = floor t
        a = cs !! high
        b = cs !! low

vecX :: Floating a => Vector a
vecX = [1,0,0]

vecY :: Floating a => Vector a
vecY = [0,1,0]

vecZ :: Floating a => Vector a
vecZ = [0,0,1]

forwardV :: Floating a => Quaternion a -> Vector a
forwardV q = rotateVQ q vecZ

rightV :: Floating a => Quaternion a -> Vector a
rightV q = rotateVQ q vecX

upV :: Floating a => Quaternion a -> Vector a
upV q = rotateVQ q vecY

rotateVQ :: Floating a => Quaternion a -> Vector a -> Vector a
rotateVQ q [vx,vy,vz] = [x,y,z]
   where Q w x y z = mulQ (mulQ q (Q 0 vx vy vz)) (conjQ q) 

--physics
dqdt :: (Floating a, Ord a) => Vector a -> Quaternion a -> Quaternion a
dqdt (x:y:z:rest) q = q'
  where qv = (Q 0.0 x y z)
        q' = scaleQ 0.5 (mulQ qv q)

distanceV :: (Floating a) => Vector a -> Vector a -> a
distanceV a b = lengthVec (subVec a b)

clampV :: (Floating a, Ord a) => a -> Vector a -> Vector a
clampV c v = if lengthVec v > c 
             then scaleVec c (normalizeV v)
             else v

inRange :: (Floating a, Ord a) => (a,a) -> a -> Bool
inRange (rmin, rmax) v = (v > rmin) && (v < rmax)
--- debug
--- unit tests
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

