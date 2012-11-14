module Main where

import Data.IORef

import Backend

import Input
import Math
import Actor

import Simulation
import Renderer

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef = do
  rndPos <- mapM (\_ -> rndSphereVec) [1..10]
  let cs = map (\p -> (Enemy "enemy" (mulScalarVec 10.0 p) identityQ (mulScalarVec 10.0 p) zeroV))  rndPos
  modifyIORef worldRef (\world -> 
     (world { actors = [newPlayer] ++ cs}))

inputAction :: InputActionPure
inputAction t world = world { actors = as, gen = nextGen1 }
  where input = worldInput world
        (lb,rb) = inputMouseButtons input
        (x:y:rest) = inputAxisL input
        (vec, nextGen) = rndPolarV (gen world)
        (vec1, nextGen1) = rndPolarV nextGen
        as = map f (actors world)

        f :: Actor -> Actor
        f player@(Player n p q v a) = Player n p q' v a'
          where ql = fromAxisAngleQ 0 0 1 ((-x)/30.0)
                q' = mulQ q ql
                a' = mulScalarVec 80.0 (mulMV [y,0.0,0.0,0.0] (toMatrixQ q))
        f actor = actor

renderActions :: [RenderAction]
renderActions = [renderer]

-- simulation is pure function. should be executed in parallel
simulateAction :: SimulateAction
simulateAction = simulate

ioActions :: IOActions
ioActions = []

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions inputAction simulateAction ioActions
