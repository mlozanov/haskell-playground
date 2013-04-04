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

  withFbo f $ do
    let pd = (shaderProgramsMap renderState) M.! "default"
    withProgram pd $ do
      attribLocation (program pd) "in_Position" $= AttribLocation 0
      attribLocation (program pd) "in_Normal" $= AttribLocation 1
      bindFragDataLocation (program pd) "Color" $= 0

      -- glGetUniformLocation :: GLuint -> Ptr GLchar -> IO GLint
      uniformProjectionMatrix <- getUniformLocation pd "projectionMatrix"
      uniformViewMatrix <- getUniformLocation pd "viewMatrix"
      uniformModelMatrix <- getUniformLocation pd "modelMatrix"

      --print uniformProjectionMatrix
      --print uniformViewMatrix
      --print uniformModelMatrix

      uniformLightPosition <- getUniformLocation pd "lightPos"
      uniformCameraPosition <- getUniformLocation pd "cameraPos"
      uniformTermCoeff <- getUniformLocation pd "termCoeff"
      uniformColorDiffuse <- getUniformLocation pd "colorDiffuse"
      uniformColorSpecular <- getUniformLocation pd "colorSpecular"
      uniformRimCoeff <- getUniformLocation pd "rimCoeff"

      --print uniformRimCoeff

      --peekArray 16 (projectionMatrix renderState) >>= print
      --peekArray 16 (viewMatrix renderState) >>= print
      --peekArray 16 (modelMatrix renderState) >>= print

      glUniformMatrix4fv (GL.getUniformLocationID uniformProjectionMatrix) 1 0 (projectionMatrix renderState)
      glUniformMatrix4fv (GL.getUniformLocationID uniformViewMatrix) 1 0 (viewMatrix renderState)
      glUniformMatrix4fv (GL.getUniformLocationID uniformModelMatrix) 1 0 (modelMatrix renderState)

      --uniformv uniformProjectionMatrix 16 (castPtr (projectionMatrix renderState) :: Ptr (TexCoord1 GLfloat))
      --uniformv uniformViewMatrix 16 (castPtr (viewMatrix renderState) :: Ptr (TexCoord1 GLfloat))
      --uniformv uniformModelMatrix 16 (castPtr (modelMatrix renderState) :: Ptr (TexCoord1 GLfloat))

      uniform uniformLightPosition $= Vertex4 200.0 0.0 100.0 (0 :: GLfloat)
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

  
  -- setup framebuffer to display render target

  -- run pass thru shader that display final image
  let fsq = (shaderProgramsMap renderState) M.! "passthru" 
  withProgram fsq $ do
    attribLocation (program fsq) "in_Position" $= AttribLocation 0
    attribLocation (program fsq) "in_Normal" $= AttribLocation 1
    bindFragDataLocation (program fsq) "Color" $= 0

    textureBinding Texture2D $= Just (textureObject f)

    texel <- getUniformLocation fsq "fb"
    uniform texel $= TextureUnit 1

    renderVbo (vboMap renderState M.! "fullscreenQuad")

    textureBinding Texture2D $= Nothing

  -- GL.lighting $= GL.Disabled
  -- GL.light (Light 0) $= GL.Disabled

  e <- GL.get GL.errors
  when (length e > 0) (print e)

  return ()


transformAndRenderVbo :: RenderState -> String -> Vector Float -> Quaternion Float -> IO ()
transformAndRenderVbo renderState n p q = draw 0.0166667 renderState (vboMap renderState M.! n)

