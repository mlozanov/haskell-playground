module Main where

import Control.Monad.State.Strict

import Data.IORef

import Backend

import Input
import Math
import Actor

import Simulation
import Renderer

type WorldState = State World World

findPlayer :: Actors -> Actor
findPlayer actors = player
  where player = head $ filter f actors

        f :: Actor -> Bool
        f (Player n p q v a) = True
        f _ = False

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef = do
  rndPos <- mapM (\_ -> rndSphereVec) [1..16]
  let cs = map (\p -> (Enemy "enemy" (mulScalarVec 20.0 p) identityQ (mulScalarVec 10.0 p) zeroV))  rndPos
  modifyIORef actorsRef (\actors -> actors ++ [newPlayer] ++ cs)


renderActions :: [RenderAction]
renderActions = [renderer]

simulate :: Actors -> World -> (Actors, World)
simulate actors world = runState state world
  where state = prepare actors >>= input >>= bullets >>= collisions >>= movement

        prepare :: Actors -> State World Actors
        prepare actors = return actors

        input :: Actors -> State World Actors
        input actors = do
          world <- get

          put $ world { gen = nextGen }

          return $ (map actorControl actors) ++ bullets

            where (Player pn pp pq pv pa) = findPlayer actors
                  (lb,rb) = inputMouseButtons (worldInput world)
                  (x:y:rest) = inputAxisL (worldInput world)
                  (vec,nextGen) = rndPolarV (gen world)

                  bullets :: Actors
                  bullets = if lb
                            then [Bullet "circle" 1.0 pp initialVelocity zeroV]
                            else []
                    where initialVelocity = mulScalarVec (300 + (lengthVec pv)) direction
                          direction = rightV pq -- right is our forward in 2d

                  actorControl :: Actor -> Actor
                  actorControl (Player n p q v a) = Player n p q' v a'
                    where ql = fromAxisAngleQ 0 0 1 ((-x)/20.0)
                          q' = mulQ q ql
                          a' = mulScalarVec 150.0 (mulMV [y,0.0,0.0] (toMatrixQ q))

                  actorControl (Enemy n p q v a) = Enemy n p q v' a
                    where v' = mulScalarVec 50.0 (mulMV vec (toMatrixQ q))

                  actorControl actor = actor

        bullets :: Actors -> State World Actors
        bullets actors = return $ filter f actors
          where f (Bullet n age p v a) | age > 0 = True
                                       | otherwise = False
                f actor = True

        collisions :: Actors -> State World Actors
        collisions actors = return actors

        movement :: Actors -> State World Actors
        movement actors = return $ map (updateActorMovement 0.016667) actors


ioActions :: IOActions
ioActions = []

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions simulate ioActions
