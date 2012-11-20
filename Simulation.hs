{-# LANGUAGE BangPatterns #-}

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

updateCameras :: World -> Cameras -> Cameras
updateCameras world cameras = undefined

collectCollisions :: World -> Actors -> [(Actor,Actor)]
collectCollisions = undefined

resolveCollisions :: World -> [(Actor,Actor)] -> Actors
resolveCollisions = undefined

updateLogic :: World -> Actors -> Actors
updateLogic world actors = undefined

updateActorMovement :: Float -> Actor -> Actor
updateActorMovement t (Player n !p !q !v !a) = Player n p' q v' a'
  where a' = zeroV
        drag = mulScalarVec (-0.007) v
        p' = euler 0.016667 p v
        nv = euler 0.016667 v a
        v' = addVec nv drag
 
updateActorMovement t (Enemy n !p !q !v !a) = Enemy n p' q' v' a'
  where a' = zeroV
        drag = mulScalarVec (-0.004) v
        nv = euler 0.016667 v a
        p' = euler 0.016667 p v
        v' = addVec nv drag
        q' = fromAxisAngleQ 0.0 0.707 0.707 (t*2)

updateActorMovement t (Bullet n age p v a) = Bullet n age' p' v' a'
  where a' = zeroV
        drag = mulScalarVec (-0.004) v
        nv = euler 0.016667 v a
        p' = euler 0.016667 p v
        v' = addVec nv drag
        age' = age - 0.016667

updateActorMovement t static@(StaticActor _ _ _) = static        

setAccelerationActor :: Vector Float -> Actor -> Actor
setAccelerationActor newAcc player@(Player _ _ _ _ a) = player { playerAcceleration = newAcc }
setAccelerationActor newAcc enemy@(Enemy _ _ _ _ a) = enemy { enemyAcceleration = newAcc }

setVelocityActor :: Vector Float -> Actor -> Actor
setVelocityActor newVel player@(Player _ _ _ v _) = player { playerVelocity = newVel }
setVelocityActor newVel enemy@(Enemy _ _ _ v _) = enemy { enemyVelocity = newVel }


