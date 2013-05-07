module Graphics where

import Graphics.Rendering.OpenGL as GL
import Foreign.Ptr
import Foreign.Marshal.Array
import Math

toGLfloat :: Float -> GLfloat
toGLfloat x = realToFrac x

toGLfloatList :: [Float] -> [GLfloat]
toGLfloatList xs = map toGLfloat xs

toPtrMatrix :: (Ptr GLfloat -> IO ()) -> Math.Matrix Float -> IO ()
toPtrMatrix fu m = withArray (toGLfloatList (Math.toList m)) fu