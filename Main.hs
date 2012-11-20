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
  rndPos <- mapM (\_ -> rndSphereVec) [1..2]
  let cs = map (\p -> (Enemy "enemy" (mulScalarVec 20.0 p) identityQ (mulScalarVec 10.0 p) zeroV))  rndPos
  modifyIORef actorsRef (\actors -> actors ++ [newPlayer] ++ cs)


renderActions :: [RenderAction]
renderActions = [renderer]

simulate :: Actors -> World -> (Actors, World)
simulate actors world = runState state world
  where state = prepare actors >>= playerInput >>= filterBullets >>= produceBullets >>= collisions >>= movement

        (Player pn pp pq pv pa) = findPlayer actors

        prepare :: Actors -> State World Actors
        prepare actors = return actors

        produceBullets :: Actors -> State World Actors
        produceBullets actors = do
          w <- get
          return $ actors ++ (bullets (worldInput w))

        bullets :: Input -> Actors
        bullets input = if lb
                        then [Bullet "circle" 2.0 pp initialVelocity zeroV]
                        else []
          where initialVelocity = mulScalarVec (300 + (lengthVec pv)) direction
                direction = rightV pq -- right is our forward in 2d
                (lb,rb) = inputMouseButtons input

        playerInput :: Actors -> State World Actors
        playerInput actors = do
          w <- get

          let (x:y:rest) = inputAxisL (worldInput w)
              (Player n p q v a):as = actors
              player = Player n p q' v a'
              ql = fromAxisAngleQ 0 0 1 ((-x)/20.0)
              q' = mulQ q ql
              a' = mulScalarVec 150.0 (mulMV [y,0.0,0.0] (toMatrixQ q))
           in return (player:as)

        filterBullets :: Actors -> State World Actors
        filterBullets actors = return $ filter f actors
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
