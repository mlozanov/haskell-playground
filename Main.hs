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

import Simulation
import Renderer

data Action = Action (IO Action)

roomVertices :: [[GLfloat]]
roomVertices = [ [x,y,z] | x <- [(-1.85),1.85], y <- [(-1.0),1.0], z <- [(-1.85),1] ]

roomNormals :: [[GLfloat]]
roomNormals = map (concat.replicate 3) ([ [(1),0,0], [(1),0,0], 
                                          [(-1),0,0], [(-1),0,0], 
                                          [0,1,0], [0,1,0], 
                                          [0,(-1),0], [0,(-1),0],
                                          [0,0,(-1)], [0,0,(-1)] ])
roomIndecies :: [GLuint]
roomIndecies = [ 0,1,2, 1,2,3  -- left
               , 4,5,6, 5,6,7  -- right
               , 0,4,1, 4,5,1  -- floor
               , 2,6,3, 6,7,3 -- ceiling
               , 6,4,0, 6,2,0 -- back
               ]

room :: [GLfloat]
room = concat $ map (\i -> (roomVertices !! (fromEnum i))  ) roomIndecies

main :: IO ()
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
  GL.pointSize  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0.18 0.18 0.18 1.0

  GL.depthFunc $= Just Lequal
   
  GL.colorMaterial $= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)

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
  sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"

  vbo <- Vbo.fromList GL.Triangles (map (* 40) room) (concat roomNormals)

  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray [defaultProgram, sphericalProgram] [vbo])

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  -- invoke the active drawing loop
  mainLoop worldRef renderStateRef renderer' simulate'

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

-- we start with waitForPress action
mainLoop :: IORef World -> IORef RenderState -> (GLfloat -> IORef World -> IORef RenderState -> IO ()) -> (GLfloat -> World -> World) -> IO ()
mainLoop world renderState render simulate = loop 0.0 world renderState waitForPress
  where 
 
    loop :: GLfloat -> IORef World -> IORef RenderState -> IO Action -> IO ()
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

    waitForPress :: IO Action
    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          return (Action waitForRelease)
 
    waitForRelease :: IO Action
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


