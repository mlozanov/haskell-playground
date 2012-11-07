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

data Input = Input { inputAxisX :: Vector Float
                   , inputAxisY :: Vector Float
                   , inputButtons :: [Bool]
                   } deriving Show

data World = World { worldTime :: Float
                   , worldInput :: Input 
                   , cameras :: Cameras
                   , actors :: Actors 
                   } deriving Show

emptyWorld :: World
emptyWorld = (World 0.0 i cs as)
    where i = Input zeroV zeroV [False, False, False, False, False, False, False, False]
          cs = [EmptyCamera]
          as = []

simulate' :: Float -> World -> World
simulate' t world = world { cameras = cs }
    where cs = updateCameras world (cameras world)
 

updateCameras :: World -> Cameras -> Cameras
updateCameras world cameras = map processOneCamera cameras
  where
    processOneCamera c = evalState (simpleFraming c) (actors world)

updateActors :: World -> Actors -> Actors
updateActors world actors = actors

