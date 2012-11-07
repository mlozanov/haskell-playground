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
import Control.Concurrent.MVar
import System.Random
import System.Mem
import System.CPUTime
import Foreign.Ptr
import Foreign.Marshal.Array

import Backend

import Graphics
import Math
import Camera
import Actor

import Vbo
import Fbo
import Shader

import Simulation
import Renderer

import Primitives

data Action = Action (IO Action)

type RenderAction = (Float -> IORef World -> IORef RenderState -> IO ())
type SimulateAction = (Float -> World -> World)

test :: [GLfloat]
test = [ x | x <- [0,0.01 .. 100.0] ]

simulationMVar :: IO (MVar World)
simulationMVar = newMVar (World 0.0 [EmptyCamera] []) 

main :: IO ()
main = do 
  setup 960 544

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  -- keep all line strokes as a list of points in an IORef
  worldRef <- newIORef (World 0.0 [EmptyCamera] [])

  projMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  viewMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  defaultProgram <- newProgram "../data/shaders/default.vert" "../data/shaders/default.frag" 
  sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"

  vbo <- Vbo.fromList GL.Triangles (map (* 40) room) (concat roomNormals)

  vboBall <- Vbo.fromList GL.Points ball ball

  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray [defaultProgram, sphericalProgram] [vbo,vboBall])

  --
  --forkIO $ forever (simulationLoop worldRef)

  -- invoke the active drawing loop
  mainLoop worldRef renderStateRef renderer'

  finalize


--simulationLoop :: IORef World -> IO ()
--simulationLoop worldRef = do
--  world <- readIORef worldRef
--  modifyIORef worldRef $ simulate' ((Simulation.time world) + 0.001)
--  return () -- force evaluation
--  threadDelay 10


-- we start with waitForPress action
mainLoop :: IORef World -> IORef RenderState -> RenderAction -> IO ()
mainLoop world renderState render = loop 0.0 world renderState waitForPress
  where 
 
    loop :: Float -> IORef World -> IORef RenderState -> IO Action -> IO ()
    loop t w r action = do
      t0 <- getCPUTime

      modifyIORef w (simulate' t)

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


