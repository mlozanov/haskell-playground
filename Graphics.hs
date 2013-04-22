module Graphics where

import Graphics.Rendering.OpenGL as GL
import Foreign.Ptr
import Foreign.Marshal.Array
import Math

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
vector3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vector3 GLfloat
vector3 = GL.Vector3

fromVector :: Vector Float -> GL.Vector3 GLfloat
fromVector [x,y,z] = vector3 (toGLfloat x) (toGLfloat y) (toGLfloat z)
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

toGLfloat :: Float -> GLfloat
toGLfloat x = realToFrac x

toGLfloatList :: [Float] -> [GLfloat]
toGLfloatList xs = map toGLfloat xs

toPtrMatrix :: (Ptr GLfloat -> IO ()) -> Math.Matrix Float -> IO ()
toPtrMatrix fu m = withArray (toGLfloatList (Math.toList m)) fu