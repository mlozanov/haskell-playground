{-# LANGUAGE FlexibleInstances #-}

module Renderer where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.ARB.ShaderObjects
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
import Foreign.C.String

import Data.IORef
import qualified Data.Map as M

import Backend

import Graphics
import Math
import Camera
import Actor

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

  draw dt renderState (Explosion n p age power) = transformAndRenderVbo renderState n p identityQ

instance Drawable Vbo where
  draw dt renderState vbo = renderVbo vbo

instance Uniform ([Float]) where
  uniform location = makeStateVar getter setter
    where setter ms = withArray (toGLfloatList ms) fu
          getter = undefined
          fu = glUniformMatrix4fv (GL.getUniformLocationID location) 1 0
  uniformv = undefined

instance Uniform (Math.Matrix Float) where
  uniform location = makeStateVar getter setter
    where setter = toPtrMatrix (\ptr -> glUniformMatrix4fv (GL.getUniformLocationID location) 1 0 ptr)
          getter = undefined
  uniformv = undefined

uniformMatrix4 :: UniformLocation -> StateVar (Ptr GLfloat)
uniformMatrix4 location = makeStateVar getter setter
  where setter = glUniformMatrix4fv (GL.getUniformLocationID location) 1 0
        getter = undefined


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

  pokeArray (projectionMatrix renderState) (toList Math.perspective)
  let vm = Math.identity `mulMM` Math.translate 0.0 0.0 (-200)
  let mm = Math.identity
  pokeArray (viewMatrix renderState) (toList vm)
  pokeArray (modelMatrix renderState) (toList mm)

  -- setup render target
  -- draw all VBOs in renderstate

  let f = (fboMap renderState) M.! "default"

  let lightX = 300.0 * sin (3.14 * 0.51 * (worldDt world) * fromIntegral (worldTime world))
  let lightY = 300.0 * cos (3.14 * 0.51 * (worldDt world) * fromIntegral (worldTime world))
  let lightZ = 100.0 * cos (3.14 * 0.4 * (worldDt world) * fromIntegral (worldTime world))

  GL.depthFunc $= Just Lequal

  activeTexture $= TextureUnit 0

  withFbo f $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    let pd = (shaderProgramsMap renderState) M.! "default"
    withProgram pd $ do
      attribLocation (program pd) "in_Position" $= AttribLocation 0
      attribLocation (program pd) "in_Normal" $= AttribLocation 1
      bindFragDataLocation (program pd) "Color" $= 0

      uniformProjectionMatrix <- getUniformLocation pd "projectionMatrix"
      uniformViewMatrix <- getUniformLocation pd "viewMatrix"
      uniformModelMatrix <- getUniformLocation pd "modelMatrix"

      uniformLightPosition <- getUniformLocation pd "lightPos"
      uniformCameraPosition <- getUniformLocation pd "cameraPos"
      uniformTermCoeff <- getUniformLocation pd "termCoeff"
      uniformColorDiffuse <- getUniformLocation pd "colorDiffuse"
      uniformColorSpecular <- getUniformLocation pd "colorSpecular"
      uniformRimCoeff <- getUniformLocation pd "rimCoeff"

      uniformMatrix4 uniformProjectionMatrix $= projectionMatrix renderState
      uniformMatrix4 uniformViewMatrix $= viewMatrix renderState
      uniformMatrix4 uniformModelMatrix $= modelMatrix renderState

      uniform uniformLightPosition $= Vertex4 (100.0 + (toGLfloat lightX)) (toGLfloat lightY) 130.0 (0 :: GLfloat)
      uniform uniformCameraPosition $= Vertex4 0 0 200 (0 :: GLfloat)
      uniform uniformTermCoeff $= Vertex4 2.0 1.0 0.0001 (0.000001 :: GLfloat)
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
  
  -- setup framebuffer to display render target

  GL.depthFunc $= Nothing

  -- run pass thru shader that display final image
  let fsq = (shaderProgramsMap renderState) M.! "passthru" 
  withProgram fsq $ do
    attribLocation (program fsq) "in_Position" $= AttribLocation 0
    attribLocation (program fsq) "in_Normal" $= AttribLocation 1
    bindFragDataLocation (program fsq) "Color" $= 0

    texel <- getUniformLocation fsq "fb"
    uniform texel $= Index1 (0 :: GLint)

    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (textureObject f)

    renderVbo (vboMap renderState M.! "fullscreenQuad")

    textureBinding Texture2D $= Nothing

  e <- GL.get GL.errors
  when (length e > 0) (print e)

  return ()


transformAndRenderVbo :: RenderState -> String -> Vector Float -> Quaternion Float -> IO ()
transformAndRenderVbo renderState n p q = do
  let pd = (shaderProgramsMap renderState) M.! "default"
  uniformModelMatrix <- getUniformLocation pd "modelMatrix"
  let mm = Math.translate (x p) (y p) (z p)

  uniform uniformModelMatrix $= mm
  draw 0.0166667 renderState (vboMap renderState M.! n)

