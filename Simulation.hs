module Simulation where

import Control.Monad.State

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

data World = World { time :: Float
                   , cameras :: Cameras
                   , actors :: Actors }


simulate' :: Float -> World -> World
simulate' t world = world { Simulation.time = t, cameras = cs }
    where cs = updateCameras world (cameras world)
 

updateCameras :: World -> Cameras -> Cameras
updateCameras world cameras = map processOneCamera cameras
  where
    processOneCamera c = evalState (simpleFraming c) (actors world)

