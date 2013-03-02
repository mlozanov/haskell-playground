module Main where

import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL

import Data.IORef
import Data.Map as M hiding (map,filter,null)

import Backend

import Primitives
import Shader
import Vbo

import Input
import Math
import Actor

import Simulation
import Renderer
import Collision

import TestFFI

type WorldState = State World World

main :: IO ()
main = testFFI 100 >> setup 1280 720 "sharpshooter" setupAction renderActions simulate ioActions

-- TODO move player outside of actor's list for better performance
findPlayer :: Actors -> Actor
findPlayer (player:actors) = player

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  rndPos <- mapM (\_ -> rndPolarVec) [1..16]
  --let rndPos = map circleVec [-pi, -(pi - (pi/16)) .. pi]
  let cs = map defaultEnemy rndPos
  --let room = StaticActor "room" zeroV identityQ
  modifyIORef actorsRef (\actors -> [newPlayer] ++ cs ++ actors)

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, bufferObjectsMap = objects } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map (* 140) room) (concat roomNormals)
  vboBall <- Vbo.fromList GL.Points ballVertices ballNormals
  vboPlayer <- Vbo.fromList GL.Points playerVertices playerNormals
  vboCircle <- Vbo.fromList GL.LineStrip (circleVertices 2.0) circleNormals

  vboPentagon <- Vbo.fromList GL.LineStrip (ngonVertices 14.0 5.0) (ngonNormals 5.0)

  vboTriangle <- Vbo.fromList GL.LineStrip (ngonVertices 5.0 3.0) (ngonNormals 3.0)  

  return $ M.fromList [("player", vboPlayer), ("circle", vboCircle), ("enemy", vboPentagon), ("room", vboRoom), ("triangle", vboTriangle)]

createShaderPrograms :: IO (Map String ShaderProgram)
createShaderPrograms = do
  defaultProgram <- newProgram "../data/shaders/default.vert" "../data/shaders/default.frag" 
  sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"  

  return $ M.fromList [("default", defaultProgram), ("spherical", sphericalProgram)]

renderActions :: [RenderAction]
renderActions = [render]

ioActions :: IOActions
ioActions = []

simulate :: Actors -> World -> (Actors, World)
simulate as w = runState state w
  where state = prepare as >>= playerInput >>= processActors >>= executeBulletCallback >>= filterBullets >>= produceBullets >>= collisions >>= movementBullets >>= movement 

        player@(Player pn pp pq pv pa psr pst) = findPlayer as

        prepare :: Actors -> State World Actors
        prepare actors = return actors

        produceBullets :: Actors -> State World Actors
        produceBullets actors = do
          world <- get
          put $ world { bullets = (bullets world) ++ newBullets (worldInput world) }
          return actors
            where newBullets :: Input -> Actors
                  newBullets input = shootOneBullet condition player
                    where (lb,rb) = inputMouseButtons input
                          condition = lb && (playerShootingTimer player <= 0.001)

        shootOneBullet :: Bool -> Actor -> Actors
        shootOneBullet b p@Player{} = [ bs | bs <- [Bullet "circle" 1.5 pp initialVelocity zeroV passthru], b ]
          where initialVelocity = mulScalarVec (200 + (lengthVec $ playerVelocity p)) direction
                direction = rightV $ playerOrientation p

        shootOneBullet b e@Enemy{} = [ bs | bs <- [Bullet "circle" 0.5 pp initialVelocity zeroV passthru], b ]
          where initialVelocity = mulScalarVec (200 + (lengthVec $ enemyVelocity e)) direction
                direction = rightV $ enemyOrientation e -- right is our forward in 2d

        explosive :: Actor -> Actors
        explosive b = explosion (bulletPosition b)

        explosion :: Vector Float -> Actors
        explosion p = map (\d -> Bullet "triangle" 4.0 p (mulScalarVec 80 d) zeroV passthru) directions
          where directions = map circleVec [-pi, -(pi - (pi/4)) .. pi]

        playerInput :: Actors -> State World Actors
        playerInput actors = do
          w <- get

          let (x:y:rest) = inputAxisL (worldInput w)
              (Player n p q v a sr st):as = actors
              player = Player n p q' v a' sr st
              ql = fromAxisAngleQ 0 0 1 ((-x)/12.0)
              q' = mulQ q ql
              a' = mulScalarVec 2200.0 (mulMV [y,0.0,0.0] (toMatrixQ q))
           in return (player:as)


        executeBulletCallback :: Actors -> State World Actors
        executeBulletCallback actors = do
          world <- get

          let bs = concat $ map (\b -> if not (bulletAge b > 0) then (bc b) b else [b]) (bullets world)
              bc b = bulletCallback b
           in put $ world { bullets = bs }

          return actors

        filterBullets :: Actors -> State World Actors
        filterBullets actors = do
          world <- get
          put $ world { bullets = filter f (bullets world) }
          return actors
            where f (Bullet n age p v a callback) | age > 0 = True
                                                  | otherwise = False

        collisions :: Actors -> State World Actors
        collisions actors = do
          world <- get

          return $ concat $ map ((\bs a -> if (not . null $ filter (f a) bs) then [] else [a]) (bullets world)) actors

            where f a@(Enemy {}) b = not $ collide ((Circle (enemyPosition a) 12.0), (Circle (bulletPosition b) 2.0))
                  f _ _ = False

        processActors :: Actors -> State World Actors
        processActors actors = do
          world <- get

          let as = map (timer world) actors
          let as' = map reset as
          return as' 

            where timer w p@Player{} = p { playerShootingTimer = (playerShootingTimer p - (worldDt w)) }
                  timer w e@Enemy{} = e { enemyShootingTimer = (enemyShootingTimer e - (worldDt w)) }
                  timer w a = a

                  reset p@Player{} = if playerShootingTimer p < (-0.01667) then p { playerShootingTimer = playerShootingRate p }
                                                                           else p
                  reset a = a

        movement :: Actors -> State World Actors
        movement actors = do
          world <- get
          return $ map (updateMovement (worldTime world)) actors

        movementBullets :: Actors -> State World Actors
        movementBullets actors = do
          world <- get
          put $ world { bullets = map (updateMovement (worldTime world)) (bullets world) }
          return actors
