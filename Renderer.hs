module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT.Objects as O
import Graphics.Rendering.OpenGL (($=))

import Control.Monad
import Control.Monad.State
import Control.Concurrent

import System.Random
import System.Mem
import System.CPUTime

import Foreign.Ptr
import Foreign.Marshal.Array

import Data.IORef
import Data.Map as M hiding (map)

import Backend

import Graphics
import Math
import Camera
import Actor

import Vbo
import Fbo
import Shader


toGLMatrix :: Math.Matrix GLfloat -> IO (GLmatrix GLfloat)
toGLMatrix m = newMatrix GL.RowMajor (Math.toList m) :: IO (GLmatrix GLfloat)

matrixFloatToGLfloat :: Math.Matrix Float -> Math.Matrix GLfloat
matrixFloatToGLfloat (M ms) = M (map realToFrac ms)  

render :: IORef World -> IORef Actors -> IORef RenderState -> IO ()
render worldRef actorsRef renderStateRef = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  renderState <- readIORef renderStateRef
  world <- readIORef worldRef 
  actors <- readIORef actorsRef

  -- projection matrix
  GL.matrixMode $= GL.Projection
  toGLMatrix Math.perspective >>= (\m -> matrix (Just GL.Projection) $= m)
  -- projection matrix

  -- view matrix
  GL.matrixMode $= GL.Modelview 0
  toGLMatrix Math.identity >>= (\m -> matrix (Just (GL.Modelview 0)) $= m)

  toGLMatrix (Math.translate 0 0 (-300)) >>= multMatrix

--  let q = normQ (fromAxisAngleQ 0 1 0 (degToRad (0.0 * sin (realToFrac t))))
--   in let m = toMatrixQ q
--       in toGLMatrix m >>= multMatrix
  -- view matrix 

  -- draw all VBOs in renderstate
  let p = (shaderProgramsMap renderState) ! "default"
  let lx = 60.0 * cos (2.0 * (realToFrac (worldTime world)))
  let ly = 50.0 * sin (realToFrac (worldTime world))
  let lz = 100.0 -- + (50.0 * sin (8.0 * t))

  withProgram p $ do
    uniformLightPosition <- getUniformLocation p "lightPos"
    uniformCameraPosition <- getUniformLocation p "cameraPos"
    uniformTermCoeff <- getUniformLocation p "termCoeff"
    uniformColorDiffuse <- getUniformLocation p "colorDiffuse"
    uniformColorSpecular <- getUniformLocation p "colorSpecular"
    uniformRimCoeff <- getUniformLocation p "rimCoeff"
  
    uniform uniformLightPosition $= Vertex4 lx ly lz (0 :: GLfloat)
    uniform uniformCameraPosition $= Vertex4 0 0 200 (0 :: GLfloat)
    uniform uniformTermCoeff $= Vertex4 0.7 0.1 0.00001 (0.0000001 :: GLfloat)
    uniform uniformColorDiffuse $= Vertex4 1 1 1 (1 :: GLfloat)
    uniform uniformColorSpecular $= Vertex4 1 1 1 (1 :: GLfloat)
    uniform uniformRimCoeff $= Vertex4 1 1 1 (1.276 :: GLfloat)

    --print $ "=================================================="
    mapM_ (renderActor renderState) actors

    uniform uniformColorDiffuse $= Vertex4 1 0 0 (1 :: GLfloat)
    uniform uniformColorSpecular $= Vertex4 1 0 0 (1 :: GLfloat)
    uniform uniformRimCoeff $= Vertex4 1 0 0 (1.276 :: GLfloat)
    mapM_ (renderActor renderState) (bullets world)
    --print $ "--------------------------------------------------"

  GL.lighting $= GL.Disabled
  GL.light (Light 0) $= GL.Disabled

  -- draw the title
  title (worldTime world)

  return ()

title t = preservingMatrix $ do 
            GL.translate $ vector3 (64.0) 0.0 (-68.0)
            GL.color $ color3 1 1 1
            GL.scale (0.38) 0.38 (0.38 :: GLfloat)
            renderString Fixed8x16 "sharpshooter"
            GL.color $ color3 1 1 1

transformAndRenderVbo :: RenderState -> String -> Vector Float -> Quaternion Float -> IO ()
transformAndRenderVbo renderState n p q = preservingMatrix $
  do GL.translate $ fromVector p
     toGLMatrix (matrixFloatToGLfloat (toMatrixQ q)) >>= multMatrix
     renderVbo (bufferObjectsMap renderState M.! n)

renderActor :: RenderState -> Actor -> IO ()
renderActor renderState (Player n p q v a) = transformAndRenderVbo renderState n p q
renderActor renderState (Enemy n p q v a _ _) = transformAndRenderVbo renderState n p q
renderActor renderState (StaticActor n p q) = transformAndRenderVbo renderState n p q
renderActor renderState (Bullet n age p v a callback) = transformAndRenderVbo renderState n p identityQ
renderActor renderState (Rocket n p) = transformAndRenderVbo renderState n p identityQ
renderActor renderState (Explosion n p age power) = transformAndRenderVbo renderState n p identityQ

