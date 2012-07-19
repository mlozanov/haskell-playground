{-# LANGUAGE RankNTypes #-}

module Main where

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

data Action = Action (IO Action)

data World = World { cameras :: Cameras
                   , actors :: Actors }

data RenderState = RenderState { projectionMatrix :: Ptr GLfloat
                               , viewMatrix :: Ptr GLfloat
                               , shaderPrograms :: [ShaderProgram] 
                               , bufferObjects :: [Vbo]
                               }

cubeVertices = [ [x,y,z] | x <- [(-1.85),1.85], y <- [(-1.0),1.0], z <- [(-1.85),1] ] :: [[GLfloat]]
cubeNormals = map (concat.replicate 3) ([ [1,0,0], [(1),0,0], 
                                          [(-1),0,0], [(-1),0,0], 
                                          [0,1,0], [0,1,0], 
                                          [0,(-1),0], [0,(-1),0],
                                          [0,0,(-1)], [0,0,(-1)] ] :: [[GLfloat]])
cubeIndecies = [ 0,1,2, 1,2,3  -- right
               , 4,5,6, 5,6,7  -- left
               , 0,4,1, 4,5,1  -- floor
               , 2,6,3, 6,7,3 -- ceiling
               , 6,4,0, 6,2,0 -- back
               ] :: [GLuint]


cube = (concat $ map (\i -> (cubeVertices !! (fromEnum i))  ) cubeIndecies)

main = do 
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 1280 690) [GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 24] GLFW.Window
  GLFW.windowTitle $= "das zimmer"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  GL.pointSize  $= 5.0
  -- set the color to clear background
  GL.clearColor $= Color4 0.18 0.18 0.18 1.0

  GL.depthFunc $= Just Less
  
  GL.ambient  (GL.Light 0) $= GL.Color4 0.2 0.2 0.2 1.0
  GL.diffuse  (GL.Light 0) $= GL.Color4 0.4 0.6 0.8 1.0
  GL.specular (GL.Light 0) $= GL.Color4 0.8 0.8 0.8 1.0
--  GL.lightModelAmbient  $= GL.Color4 1.0 1.0 1.0 1.0
 
  GL.colorMaterial $= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)

  GL.lighting $= GL.Enabled
  GL.light (GL.Light 0) $= GL.Enabled
  GL.position (GL.Light 0) $= GL.Vertex4 100.0 0.0 (0.0) 1.0
 
  -- set 2D perspective view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
       do GL.viewport   $= (GL.Position 0 0, size)
          GL.matrixMode $= GL.Projection
          GL.loadIdentity

          let m' = Math.toList Math.perspective
          p <- newMatrix GL.RowMajor m' :: IO (GLmatrix GLfloat)
          multMatrix p
 
  -- keep all line strokes as a list of points in an IORef
  worldRef <- newIORef (World [EmptyCamera] [])

  projMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  viewMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  defaultProgram <- newProgram "../data/shaders/default.vert" "../data/shaders/default.frag" 
  badPrintProgram <- newProgram "../data/shaders/badprint.vert" "../data/shaders/badprint.frag" 
  sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"

  vbo <- Vbo.fromList GL.Triangles (map (* 40) cube) (concat cubeNormals)

  vboPoints <- Vbo.fromList GL.Points (map (* 40.0) (concat [[x,y,z] | x <- [(-1.0), (-0.93) .. 1.0], y <- [(-1.0), (-0.93) .. 1.0], z <- [(-1.0), (-0.9) .. 1.0], sqrt (x*x + y*y + z*z) < 1.0 ])) []

  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray [defaultProgram, badPrintProgram, sphericalProgram] [vbo, vboPoints])

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  -- invoke the active drawing loop
  mainLoop worldRef renderStateRef renderer' simulate'

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

updateCameras cameras state = loop 
  where
    loop = map (\c -> evalState (simpleFraming c) state) cameras

-- we start with waitForPress action
mainLoop world renderState render simulate = loop 0.0 world renderState waitForPress
  where 
 
    loop t w r action = do
      t0 <- getCPUTime

      modifyIORef w $ simulate t
      render t w r
      GLFW.swapBuffers

      performGC

      t1 <- getCPUTime

      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
          -- perform action
          Action action' <- action

          -- sleep for 1ms to yield CPU to other applications
          GLFW.sleep 0.001

          -- only continue when the window is not closed
          windowOpenStatus <- getParam Opened
          unless (not windowOpenStatus) $
            loop (t + 0.01) w r action' -- loop with next action

    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)

toGLMatrix :: Math.Matrix GLfloat -> IO (GLmatrix GLfloat)
toGLMatrix m = newMatrix GL.RowMajor (Math.toList m) :: IO (GLmatrix GLfloat)

renderer' :: GLfloat -> IORef World -> IORef RenderState -> IO ()
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

  let q = normQ (fromAxisAngleQ 0 1 0 (degToRad (0.0 * sin t)))
   in let m = toMatrixQ q
       in toGLMatrix m >>= multMatrix

  toGLMatrix (Math.translate (0 * sin t) 0 (-20)) >>= multMatrix
  -- view matrix 

  --GL.lighting $= GL.Enabled
  --GL.light (Light 0) $= GL.Enabled
  GL.lighting $= GL.Disabled
  GL.light (Light 0) $= GL.Disabled

  let vbo = (bufferObjects renderState) !! 0
  let vbo' = (bufferObjects renderState) !! 1

  withProgram ((shaderPrograms renderState) !! 0) (f3 vbo t)
  
  withProgram ((shaderPrograms renderState) !! 0) (f3 vbo' t)

  --withProgram ((shaderPrograms renderState) !! 0) (f2 t)

  --withProgram ((shaderPrograms renderState) !! 0) (f2' t)

  --withProgram ((shaderPrograms renderState) !! 0) (cube' t)

  GL.lighting $= GL.Disabled
  GL.light (Light 0) $= GL.Disabled

  title t

  return ()


title t = preservingMatrix $ do 
            GL.translate $ vector3 (-16.0) 0.0 0.0
            GL.color $ color3 1 1 1
            GL.scale (-0.04) 0.04 (0.04 :: GLfloat)
            renderString Fixed8x16 "DAS.ZIMMER"
            GL.color $ color3 1 1 1

f3 vbo t = preservingMatrix $ do
             GL.translate $ vector3 (0.0) 0.0 (-90.0)
             GL.rotate (10 * sin t) (vector3 0 1 1)
             renderVbo vbo

f2 t = preservingMatrix $ do
         GL.translate $ vector3 (10.0) 0.0 0.0
         GL.rotate (45 * sin t) (vector3 1 0 0)
         O.renderObject O.Solid (O.Teapot 4.0)

f2' t = preservingMatrix $ do
          GL.translate $ vector3 (-10.0) 0.0 0.0
          GL.rotate (90 * sin t) (vector3 1 0 0)
          O.renderObject O.Solid (O.Cube 4.0)

simulate' :: GLfloat -> World -> World
simulate' t world = world { cameras = cs }
    where cs = updateCameras (cameras world) (actors world)
 
