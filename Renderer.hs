module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT.Objects as O
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import System.Random
import System.Mem
import System.CPUTime
import Foreign.Ptr
import Foreign.Marshal.Array


import Graphics
import Math
import Camera
import Actor

import Vbo
import Fbo
import Shader

import Simulation

data RenderState = RenderState { projectionMatrix :: Ptr GLfloat
                               , viewMatrix :: Ptr GLfloat
                               , shaderPrograms :: [ShaderProgram] 
                               , bufferObjects :: [Vbo]
                               }


toGLMatrix :: Math.Matrix GLfloat -> IO (GLmatrix GLfloat)
toGLMatrix m = newMatrix GL.RowMajor (Math.toList m) :: IO (GLmatrix GLfloat)


renderer' :: Float -> IORef World -> IORef RenderState -> IO ()
renderer' t worldRef renderStateRef = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  renderState <- readIORef renderStateRef
  world <- readIORef worldRef 

  -- projection matrix
  GL.matrixMode $= GL.Projection
  toGLMatrix Math.perspective >>= (\m -> matrix (Just GL.Projection) $= m)
  -- projection matrix

  -- view matrix
  GL.matrixMode $= GL.Modelview 0
  toGLMatrix Math.identity >>= (\m -> matrix (Just (GL.Modelview 0)) $= m)

  toGLMatrix (Math.translate 0 0 (-100)) >>= multMatrix

  let q = normQ (fromAxisAngleQ 0 1 0 (degToRad (0.0 * sin (realToFrac t))))
   in let m = toMatrixQ q
       in toGLMatrix m >>= multMatrix
  -- view matrix 

  ([jlx,jly,jrx,jry]) <- GL.get $ GLFW.joystickPos (GLFW.Joystick 0) 4
  bs <- GL.get $ GLFW.joystickButtons (GLFW.Joystick 0)

  -- draw all VBOs in renderstate
  let p = shaderPrograms renderState !! 0
  let lx = 60.0 * cos (2.0 * (realToFrac t))
  let ly = 50.0 * sin (realToFrac t)
  let lz = 100.0 -- + (50.0 * sin (8.0 * t))
  mapM (\vbo -> withProgram p (do uniformLightPosition <- getUniformLocation p "lightPos"
                                  uniformCameraPosition <- getUniformLocation p "cameraPos"
                                  uniformTermCoeff <- getUniformLocation p "termCoeff"
                                  uniformColorDiffuse <- getUniformLocation p "colorDiffuse"
                                  uniformColorSpecular <- getUniformLocation p "colorSpecular"
  
                                  uniform uniformLightPosition $= Vertex4 lx ly lz (0 :: GLfloat)
                                  uniform uniformCameraPosition $= Vertex4 0 0 100 (0 :: GLfloat)
                                  uniform uniformTermCoeff $= Vertex4 0.7 0.1 0.001 (0.0001 :: GLfloat)
                                  uniform uniformColorDiffuse $= Vertex4 1 1 1 (1 :: GLfloat)
                                  uniform uniformColorSpecular $= Vertex4 1 1 1 (1 :: GLfloat)
                                  animate t vbo)) 
           (bufferObjects renderState)

  -- withProgram ((shaderPrograms renderState) !! 0) (animateCube t)
  
  GL.lighting $= GL.Disabled
  GL.light (Light 0) $= GL.Disabled

  -- draw the title
  title t

  return ()

title t = preservingMatrix $ do 
            GL.translate $ vector3 (54.0) 0.0 (-68.0)
            GL.color $ color3 1 1 1
            GL.scale (0.22) 0.22 (0.22 :: GLfloat)
            renderString Fixed8x16 "DAS.ZIMMER"
            GL.color $ color3 1 1 1

animate t vbo = preservingMatrix $ do
                  GL.translate $ vector3 0 0 0
                  renderVbo vbo

animateCube t = preservingMatrix $ do
                  GL.translate $ vector3 30 0 (0.0)
                  --GL.rotate (180.0 * sin t) (vector3 1 0.2 0)
                  O.renderObject O.Solid (O.Torus 10.0 20.0 16 32)

