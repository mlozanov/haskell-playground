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
import qualified Data.Map as M

import Backend

import Graphics
import Math
import Camera
import Actor
import Ai

import Vbo
import Fbo
import Shader

class Drawable a where
  draw :: Float -> RenderState -> a -> IO ()


instance Drawable Actor where
  draw dt renderState p@Player{} = transformAndRenderVbo renderState (playerName p) (playerPosition p) (playerOrientation p)

  draw dt renderState e@Enemy{} = transformAndRenderVbo renderState (enemyName e) (enemyPosition e) (enemyOrientation e)

  draw dt renderState (StaticActor n p q tag) = transformAndRenderVbo renderState n p q

  draw dt renderState (Bullet n tag age p v a callback) = transformAndRenderVbo renderState n p identityQ

  draw dt renderState r@Rocket{} = transformAndRenderVbo renderState (rocketName r) (rocketPosition r) identityQ

  draw dt renderState (Explosion n p age power) = preservingMatrix $
    do GL.translate $ fromVector p
       GL.scale (3.0 - toGLfloat age) (3.0 - toGLfloat age) (3.0 - toGLfloat age)
       toGLMatrix (matrixFloatToGLfloat (toMatrixQ identityQ)) >>= multMatrix
       renderVbo (bufferObjectsMap renderState M.! n)


instance Drawable Vbo where
  draw dt renderState vbo = renderVbo vbo


instance Drawable Seed where
  draw dt renderState seed = transformAndRenderVbo renderState "seed" (seedPosition seed) identityQ


{-# INLINE toGLMatrix #-}
{-# INLINE matrixFloatToGLfloat #-}

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

  pokeArray (projectionMatrix renderState) (toList Math.perspective)
  -- projection matrix

  -- view matrix
  GL.matrixMode $= GL.Modelview 0
  toGLMatrix Math.identity >>= (\m -> matrix (Just (GL.Modelview 0)) $= m)

  let modelMatrix' = Math.translate 0 0 (-400)
  toGLMatrix modelMatrix' >>= multMatrix

  pokeArray (modelMatrix renderState) (toList modelMatrix')

  --let q = normQ (fromAxisAngleQ 0 1 0 (degToRad (45.0)))
  -- in let m = toMatrixQ q
  --     in toGLMatrix m >>= multMatrix
  -- view matrix 

  -- draw all VBOs in renderstate
  let p = (shaderProgramsMap renderState) M.! "default"
  let lx = 60.0 * cos (0.02 * (realToFrac (worldTime world)))
  let ly = 50.0 * sin (0.02 * (realToFrac (worldTime world)))
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
    uniform uniformTermCoeff $= Vertex4 0.7 0.1 0.0001 (0.000001 :: GLfloat)
    uniform uniformColorDiffuse $= Vertex4 1 1 1 (1 :: GLfloat)
    uniform uniformColorSpecular $= Vertex4 1 1 1 (1 :: GLfloat)

    uniform uniformRimCoeff $= Vertex4 1 1 1 (1.276 :: GLfloat)
    mapM_ (draw (worldDt world) renderState) (filter (not . isStatic) actors)

    uniform uniformRimCoeff $= Vertex4 0.2 0.2 0.2 (1.276 :: GLfloat)
    mapM_ (draw (worldDt world) renderState) (filter isStatic actors)

    uniform uniformRimCoeff $= Vertex4 0.0 0.0 0.0 (1.276 :: GLfloat)
    mapM_ (draw (worldDt world) renderState) (filter (\b -> bulletTag b == Ally) (bullets world))

    uniform uniformRimCoeff $= Vertex4 0.0 0.0 0.0 (1.276 :: GLfloat)
    mapM_ (draw (worldDt world) renderState) (filter (\b -> bulletTag b == Opponent) (bullets world))

  GL.lighting $= GL.Disabled
  GL.light (Light 0) $= GL.Disabled

  -- draw the title
  title (worldTime world)

  return ()

title t = preservingMatrix $ do 
            GL.translate $ vector3 (64.0) 0.0 (-58.0)
            GL.color $ color3 1 1 1
            GL.scale 0.6 0.6 (0.60 :: GLfloat)
            renderString Fixed8x16 "sharpshooter"
            GL.color $ color3 1 1 1

transformAndRenderVbo :: RenderState -> String -> Vector Float -> Quaternion Float -> IO ()
transformAndRenderVbo renderState n p q = preservingMatrix $
  do GL.translate $ fromVector p
     toGLMatrix (matrixFloatToGLfloat (toMatrixQ q)) >>= multMatrix
     draw 0.0166667 renderState (bufferObjectsMap renderState M.! n)

