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

data Action = Action (IO Action)

data World = World { cameras :: Cameras
                   , actors :: Actors }

data RenderState = RenderState { projectionMatrix :: Ptr GLfloat
                               , viewMatrix :: Ptr GLfloat }

main = do 
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 1280 720) [GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 24] GLFW.Window
  GLFW.windowTitle $= "cine-o-matic"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0

  GL.depthFunc $= Just Less
  
  GL.ambient  (GL.Light 0) $= GL.Color4 0.3 0.3 0.3 1.0
  GL.diffuse  (GL.Light 0) $= GL.Color4 1.0 1.0 1.0 1.0
  GL.specular (GL.Light 0) $= GL.Color4 0.8 0.8 0.8 1.0
  GL.lightModelAmbient  $= GL.Color4 0.2 0.2 0.2 1.0
 
  GL.colorMaterial $= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)

  GL.lighting $= GL.Enabled
  GL.light (GL.Light 0) $= GL.Enabled
  GL.position (GL.Light 0) $= GL.Vertex4 0 10 2.0 1.0
 
  -- set 2D orthogonal view inside windowSizeCallback because
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
  worldRef <- newIORef (World [EmptyCamera] (concat $ replicate 3 ([SimpleActor, Actor [], Actor [], SimpleActor])))

  projMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  viewMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray)

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  -- invoke the active drawing loop
  mainLoop worldRef renderer' simulate'

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

updateCameras cameras state = loop 
  where
    loop = map (\c -> evalState (simpleFraming c) state) cameras
      --loop cameras state

-- we start with waitForPress action
mainLoop world render simulate = loop 0.0 world waitForPress
  where 
 
    loop t w action = do
      t0 <- getCPUTime

      modifyIORef w $ simulate t
      render t w
      GLFW.swapBuffers

      performGC

      t1 <- getCPUTime

      --print $ fromIntegral (t1 - t0) / 10^6

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
            loop (t + 0.01) w action' -- loop with next action

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

renderer' :: GLfloat -> IORef World -> IO ()
renderer' t worldRef = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  toGLMatrix Math.perspective >>= (\m -> matrix (Just GL.Projection) $= m)

  GL.lighting $= GL.Enabled
  GL.light (Light 0) $= GL.Enabled

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  -- view matrix
  let q = normQ (fromAxisAngleQ 0 1 0 (degToRad (60.0 * sin t)))
   in let m = toMatrixQ q
       in toGLMatrix m >>= multMatrix

  toGLMatrix (Math.translate (20 * sin t) 0 (-30)) >>= multMatrix
  -- view matrix 

  world <- readIORef worldRef 

  --GL.lighting $= GL.Disabled
  --GL.light (Light 0) $= GL.Disabled

  preservingMatrix $ do 
    GL.translate $ vector3 0.0 (-3.0) 0.0
    GL.color $ color3 1 0 0
    GL.scale (-0.02) 0.02 (0.02 :: GLfloat)
    renderString Fixed8x16 "cine demo!"

  GL.translate $ vector3 (-18.0) 0 0

  --GL.lighting $= GL.Enabled
  --GL.light (Light 0) $= GL.Enabled

  mapM (\a -> do GL.translate $ vector3 3.0 0 0 
                 Actor.draw a) 
       $ actors world

  --GL.lighting $= GL.Disabled
  --GL.light (Light 0) $= GL.Disabled

  return ()


simulate' :: GLfloat -> World -> World
simulate' t world = world { cameras = cs }
    where cs = updateCameras (cameras world) (actors world)
 
