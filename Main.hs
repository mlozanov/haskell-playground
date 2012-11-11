{-# LANGUAGE RankNTypes #-}

module Main where

import Data.IORef

import Backend

import Math
import Actor

import Simulation
import Renderer

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef = do
  rndPos <- mapM (\_ -> rndPolar2dVec) [1..10]
  let cs = map (\p -> (Enemy "enemy" (mulScalarVec 10.0 p) identityQ (mulScalarVec 10.0 p) zeroV))  rndPos
  modifyIORef worldRef (\world -> 
     (world { actors = [newPlayer, (Enemy "enemy" zeroV (fromAxisAngleQ 1 0 0 (pi/8)) zeroV zeroV)] ++ cs}))

renderAction :: RenderAction
renderAction = renderer

-- simulation is pure function. should be executed in parallel
simulateAction :: SimulateAction
simulateAction = simulate

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderAction simulateAction
