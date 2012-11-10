{-# LANGUAGE RankNTypes #-}

module Main where

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

import Vbo
import Fbo
import Shader

import Simulation
import Renderer

import Primitives

type RenderAction = (Float -> IORef World -> IORef RenderState -> IO ())
type SimulateAction = (Float -> World -> World)

main :: IO ()
main = do 
  -- keep all line strokes as a list of points in an IORef
  worldRef <- newIORef emptyWorld

  setup 1280 720 worldRef

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  projMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  viewMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  defaultProgram <- newProgram "../data/shaders/default.vert" "../data/shaders/default.frag" 
  --sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"

  --vbo <- Vbo.fromList GL.Triangles (map (* 40) room) (concat roomNormals)

  vboBall <- Vbo.fromList GL.Points ballVertices ballNormals
  vboPlayer <- Vbo.fromList GL.Points playerVertices playerNormals
  vboCircle <- Vbo.fromList GL.LineStrip (circleVertices 5.0) circleNormals

  let objects = [("player", vboPlayer), ("circle", vboCircle), ("enemy", vboBall)]

  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray [defaultProgram] (M.fromList objects))

  modifyIORef worldRef (\world -> 
     (world { actors = [newPlayer, newEnemy, newEnemy1, newEnemy2, (Enemy "circle" zeroV (fromAxisAngleQ 1 0 0 (pi/8)) zeroV [0,0,0,0])] ++ (replicate 500 (Enemy "circle" zeroV (fromAxisAngleQ 1 0 0 (pi/8)) zeroV [0,0,0,0]))}))

  -- invoke the active drawing loop
  mainLoop worldRef renderStateRef renderer'

  finalize

mainLoop :: IORef World -> IORef RenderState -> RenderAction -> IO ()
mainLoop world renderState render = loop 0.0 world renderState
  where 
 
    loop :: Float -> IORef World -> IORef RenderState -> IO ()
    loop t worldRef renderStateRef = do
      t0 <- getCPUTime

      joystickCallback worldRef

      modifyIORef worldRef (simulate' t)

      render t worldRef renderStateRef
      
      GLFW.swapBuffers

      --world <- readIORef worldRef
      --print $ worldInput world

      --performGC

      t1 <- getCPUTime

      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
          -- sleep for 1ms to yield CPU to other applications
          GLFW.sleep 0.001

          -- only continue when the window is not closed
          windowOpenStatus <- getParam Opened
          unless (not windowOpenStatus) $
            loop (t + 0.01) worldRef renderStateRef -- loop with next action



