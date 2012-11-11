{-# LANGUAGE RankNTypes #-}

module Main where

import Data.IORef

import Backend

import Math
import Actor

import Simulation
import Renderer

setupAction :: SetupAction
setupAction worldRef = do
  modifyIORef worldRef (\world -> 
     (world { actors = [newPlayer, newEnemy, newEnemy1, newEnemy2, (Enemy "circle" zeroV (fromAxisAngleQ 1 0 0 (pi/8)) zeroV [0,0,0,0])] ++ (replicate 400 (Enemy "circle" zeroV (fromAxisAngleQ 1 0 0 (pi/8)) zeroV [0,0,0,0]))}))

renderAction :: RenderAction
renderAction = renderer'

simulateAction :: SimulateAction
simulateAction = simulate'

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderAction simulateAction
