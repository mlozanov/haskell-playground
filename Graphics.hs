module Graphics where

import Graphics.Rendering.OpenGL as GL


vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
vector3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vector3 GLfloat
vector3 = GL.Vector3
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3

toGLfloat :: Float -> GLfloat
toGLfloat x = realToFrac x

