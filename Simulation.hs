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

import Backend

import Graphics
import Math
import Camera
import Actor

addActorToWorld :: World -> Actor -> World
addActorToWorld w a = w { actors = (actors w) ++ [a] }

simulate :: Float -> World -> World
simulate t world = world { worldTime = t, cameras = cs, actors = as }
    where cs = {-# SCC "updateCameras" #-} updateCameras world (cameras world)
          as = {-# SCC "updateMovement" #-} updateMovement world (actors world)
 

updateCameras :: World -> Cameras -> Cameras
updateCameras world cameras = map processOneCamera cameras
  where
    processOneCamera c = evalState (simpleFraming c) (actors world)

updateActors :: World -> Actors -> Actors
updateActors world actors = undefined

updateMovement :: World -> Actors -> Actors
updateMovement w actors = map (updateActorMovement (worldTime w)) actors

collectCollisions :: World -> Actors -> [(Actor,Actor)]
collectCollisions = undefined

resolveCollisions :: World -> [(Actor,Actor)] -> Actors
resolveCollisions = undefined

updateLogic :: World -> Actors -> Actors
updateLogic world actors = undefined

updateActorMovement :: Float -> Actor -> Actor
updateActorMovement t player@(Player n p q v a) = Player n p' q' v' a' --player { playerPosition = p', playerVelocity = v', playerAcceleration = zeroV }
  where v' = addVec (euler 0.016667 v a) (mulScalarVec (-0.005) v)
        p' = euler 0.016667 p v
        a' = zeroV
        q' = q

updateActorMovement t enemy@(Enemy n p q v a) = Enemy n p' q' v' a'
  where v' = addVec (euler 0.016667 v a) (mulScalarVec (-0.005) v)
        p' = euler 0.016667 p v
        q' = (fromAxisAngleQ 0 1 0 (t*10))
        a' = zeroV

accelerateActor :: Vector Float -> Actor -> Actor
accelerateActor newAcc player@(Player _ _ _ _ a) = player { playerAcceleration = newAcc }
accelerateActor newAcc enemy@(Enemy _ _ _ _ a) = enemy { enemyAcceleration = newAcc }

setVelocityActor :: Vector Float -> Actor -> Actor
setVelocityActor newVel player@(Player _ _ _ v _) = player { playerVelocity = newVel }
setVelocityActor newVel enemy@(Enemy _ _ _ v _) = enemy { enemyVelocity = newVel }


