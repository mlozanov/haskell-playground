module Main where

import Data.IORef

import Backend

import Input
import Math
import Actor

import Simulation
import Renderer

findPlayer :: Actors -> Actor
findPlayer actors = player
  where player = head $ filter f actors

        f :: Actor -> Bool
        f (Player n p q v a) = True
        f _ = False

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef = do
  rndPos <- mapM (\_ -> rndSphereVec) [1..16]
  let cs = map (\p -> (Enemy "enemy" (mulScalarVec 20.0 p) identityQ (mulScalarVec 10.0 p) zeroV))  rndPos
  modifyIORef worldRef (\world -> 
     (world { actors = [newPlayer] ++ cs}))

inputAction :: InputActionPure
inputAction t world = world { actors = actors' ++ bullets, gen = nextGen }
  where input = worldInput world
        (lb,rb) = inputMouseButtons input
        (x:y:rest) = inputAxisL input
        (vec,nextGen) = rndPolarV (gen world)

        actors' = map movement (actors world)

        bullets = case lb of
                    True -> [Bullet "circle" pp initialVelocity zeroV]
                    False -> []
          where (Player pn pp pq pv pa) = findPlayer (actors world)
                initialVelocity = mulScalarVec 10 pv

        movement :: Actor -> Actor
        movement (Player n p q v a) = Player n p q' v a'
          where ql = fromAxisAngleQ 0 0 1 ((-x)/30.0)
                q' = mulQ q ql
                a' = mulScalarVec 150.0 (mulMV [y,0.0,0.0] (toMatrixQ q))

        movement (Enemy n p q v a) = Enemy n p q v' a
          where v' = mulScalarVec 50.0 (mulMV vec (toMatrixQ q))

        movement actor = actor

renderActions :: [RenderAction]
renderActions = [renderer]

-- simulation is pure function. should be executed in parallel
simulateAction :: SimulateAction
simulateAction = simulate

simulate :: Float -> World -> World 
simulate t world = world { worldTime = t, actors = actors' }
    where actors' = map (updateActorMovement t) (actors world)

ioActions :: IOActions
ioActions = []

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions inputAction simulateAction ioActions
