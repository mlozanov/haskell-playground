{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Math where

import Graphics.Rendering.OpenGL (GLfloat, GLmatrix)

type Vec4 = [GLfloat]
type Matrix44 = [Vec4]

addVec :: Vec4 -> Vec4 -> Vec4
addVec = zipWith (+)

subVec :: Vec4 -> Vec4 -> Vec4
subVec = zipWith (-)

innerVec :: Vec4 -> Vec4 -> Vec4
innerVec = zipWith (*)

negateVec :: Vec4 -> Vec4
negateVec = map negate

dotVec :: Vec4 -> Vec4 -> GLfloat
dotVec a b = sum $ innerVec a b

lengthVec :: Vec4 -> GLfloat
lengthVec a = sqrt . sum $ map square a
    where square x = x*x

degToRad a = a * pi / 180.0
radToDeg a = a * 180.0 / pi

glMatrix :: Matrix44 -> [GLfloat]
glMatrix m = concat m

class Matrix a where
    identity :: a
    frustum :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> a
    perspective :: a
    ortho :: a
    translate :: GLfloat -> GLfloat -> GLfloat -> a
    rotate :: GLfloat -> GLfloat -> GLfloat -> a

instance Matrix Matrix44 where
    identity = [ [1,0,0,0], [0,1,0,0], [0,0,1,0], [0,0,0,1] ]

    frustum left right top bottom near far = [ [x, 0, a, 0]
                                             , [0, y, b, 0]
                                             , [0, 0, c, d]
                                             , [0, 0, -1, 0] ]
        where x = 2*near/(right-left)
              y = 2*near/(top-bottom)
              a = (right+left)/(right-left)
              b = (top+bottom)/(top-bottom)
              c = -(far+near)/(far-near)
              d = -2*far*near/(far-near)

    perspective = frustum left right top bottom near far
        where near = 1.0
              far = 2000.0
              left = top * aspect
              right = bottom * aspect
              top = near * tan (60.0 * pi / 360.0)
              bottom = -top
              aspect = 1.78

    ortho = identity

    translate x y z = identity

    rotate x y z = identity


lerp :: GLfloat -> Vec4 -> Vec4 -> Vec4
lerp x a b = addVec a' b'
    where a' = innerVec a $ replicate 4 (1-x)
          b' = innerVec b $ replicate 4 x
