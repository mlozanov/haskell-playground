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

instance Physical Actor where
    updateMovement dt (Player n !p !q !v !a sr st) = Player n p' q v' a' sr st
      where a' = zeroV
            drag = mulScalarVec (-0.3) v
            p' = euler dt p v
            nv = euler dt v a
            v' = addVec nv drag
            q' = normQ $ addQ q (scaleQ dt (dqdt [0.0, 0.0, 10.0] q))
            
    updateMovement dt (Enemy n !p !q !v !a sr sp st) = Enemy n p' q' v' a' sr sp st
      where a' = zeroV
            drag = mulScalarVec (-0.001) v
            nv = euler dt v a
            p' = euler dt p v
            v' = addVec nv drag
            q' = normQ $ addQ q (scaleQ dt (dqdt [0.0, 0.0, 1.0] q))

    updateMovement dt (Bullet n tag age p v a callback) = Bullet n tag age' p' v' a' callback
      where a' = zeroV
            drag = mulScalarVec (-0.001) v
            nv = euler dt v a
            p' = euler dt p v
            v' = addVec nv drag
            age' = age - dt

    updateMovement dt static@StaticActor{} = static

    updateMovement dt actor = actor

setAccelerationActor :: Vector Float -> Actor -> Actor
setAccelerationActor newAcc player@Player{} = player { playerAcceleration = newAcc }
setAccelerationActor newAcc enemy@Enemy{} = enemy { enemyAcceleration = newAcc }

setVelocityActor :: Vector Float -> Actor -> Actor
setVelocityActor newVel player@Player{} = player { playerVelocity = newVel }
setVelocityActor newVel enemy@Enemy{} = enemy { enemyVelocity = newVel }

