module Main where

import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL

import Data.IORef
import Data.Map as M hiding (map,filter)

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

type WorldState = State World World

findPlayer :: Actors -> Actor
findPlayer (player:actors) = player

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  --rndPos <- mapM (\_ -> rndPolarVec) [1..16]
  rndPos <- mapM circleVec [-pi, -(pi - (pi/16)) .. pi]
  let cs = map (\p -> (Enemy "enemy" (mulScalarVec 120.0 p) identityQ (mulScalarVec 15.0 p) zeroV))  rndPos
  let room = StaticActor "room" zeroV identityQ
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

  vboPentagon <- Vbo.fromList GL.LineStrip (ngonVertices 16.0 5.0) (ngonNormals 5.0)

  vboTriangle <- Vbo.fromList GL.LineStrip (ngonVertices 5.0 3.0) (ngonNormals 3.0)  

  return $ M.fromList [("player", vboPlayer), ("circle", vboCircle), ("enemy", vboPentagon), ("room", vboRoom), ("triangle", vboTriangle)]

createShaderPrograms :: IO (Map String ShaderProgram)
createShaderPrograms = do
  defaultProgram <- newProgram "../data/shaders/default.vert" "../data/shaders/default.frag" 
  sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"  

  return $ M.fromList [("default", defaultProgram), ("spherical", sphericalProgram)]

renderActions :: [RenderAction]
renderActions = [render]

simulate :: Actors -> World -> (Actors, World)
simulate as w = runState state w
  where state = prepare as >>= playerInput >>= filterBullets >>= produceBullets >>= movement 

        (Player pn pp pq pv pa) = findPlayer as

        prepare :: Actors -> State World Actors
        prepare actors = return actors

        produceBullets :: Actors -> State World Actors
        produceBullets actors = do
          world <- get
          put $ world { bullets = (bullets world) ++ newBullets (worldInput world) }
          return actors
            where newBullets :: Input -> Actors
                  newBullets input = if lb
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
              ql = fromAxisAngleQ 0 0 1 ((-x)/12.0)
              q' = mulQ q ql
              a' = mulScalarVec 2200.0 (mulMV [y,0.0,0.0] (toMatrixQ q))
           in return (player:as)

        filterBullets :: Actors -> State World Actors
        filterBullets actors = do
          world <- get
          put $ world { bullets = filter f (bullets world) }
          return actors
            where f (Bullet n age p v a) | age > 0 = True
                                         | otherwise = False

        collisions :: Actors -> State World Actors
        collisions actors = do
          return actors'
            where pairs = [ (a1, a2) | a1@(Bullet n age p v a) <- actors, a2 <- actors ]
                  actors' = actors

        movement :: Actors -> State World Actors
        movement actors = do
          world <- get
          put $ world { bullets = map (updateMovement (worldTime world)) (bullets world) }
          return $ map (updateMovement (worldTime world)) actors

ioActions :: IOActions
ioActions = []

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions simulate ioActions
