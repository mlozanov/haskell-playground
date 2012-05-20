module Math where

type Vec4 = [Float]
type Vec3 = [Float]
type Matrix44 = [Vec4]

addVec :: Vec4 -> Vec4 -> Vec4
addVec = zipWith (+)

subVec :: Vec4 -> Vec4 -> Vec4
subVec = zipWith (-)

innerVec :: Vec4 -> Vec4 -> Vec4
innerVec = zipWith (*)

negateVec :: Vec4 -> Vec4
negateVec = map negate

dotVec :: Vec4 -> Vec4 -> Float
dotVec a b = sum $ innerVec a b

lengthVec :: Vec4 -> Float
lengthVec a = sqrt . sum $ map square a
    where square x = x*x

degToRad a = a * pi / 180.0
radToDeg a = a * 180.0 / pi

