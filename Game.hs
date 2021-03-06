module Game (setupAction,
             renderActions,
             simulate,
             ioActions)
  where

import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL

import Data.IORef
import Data.Map as M hiding (map,filter,null)

import Backend

import Primitives
import Shader
import Vbo
import Fbo

import Input
import Math
import Actor

import Simulation
import Renderer
import Collision

import Timesheet

import Behaviours

import Ai

-- import TestFFI

type WorldState = State World World

newtype Simulation = Simulation (StateT World IO ())

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions simulate ioActions

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  let room = StaticActor "room" zeroV identityQ Type1

  backgroundActorPositions <- mapM (\_ -> rndVec) [1..256]
  let bs = map (\p -> StaticActor "square" (scaleVec 600.0 (addVec [0.0, 0.0, -0.4] $ mulVec [1.0, 0.5, 1.0] p)) identityQ Type2) backgroundActorPositions
  modifyIORef actorsRef (\actors -> [newPlayer] ++ bs ++ actors)

  --modifyIORef actorsMapRef (\m -> M.insert m bs)

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createFramebuffers

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map (* 140) room) (concat roomNormals)
  vboBall <- Vbo.fromList GL.Points ballVertices ballNormals
  vboPlayer <- Vbo.fromList GL.Points playerVertices playerNormals
  vboCircle <- Vbo.fromList GL.LineStrip (circleVertices 2.0) circleNormals

  vboPentagon <- Vbo.fromList GL.TriangleStrip (ngonVertices 14.0 5.0) (ngonNormals 5.0)

  vboTriangle <- Vbo.fromList GL.LineStrip (ngonVertices 3.5 3.0) (ngonNormals 3.0)

  vboSquare <- Vbo.fromList GL.LineStrip (ngonVertices 8.0 4.0) (ngonNormals 4.0)

  vboExplosition <- Vbo.fromList GL.LineStrip (ngonVertices 15.0 8.0) (ngonNormals 8.0)

  vboSmallExplosition <- Vbo.fromList GL.LineStrip (ngonVertices 1.0 6.0) (ngonNormals 6.0)

  vboFullscreenQuad <- Vbo.fromList GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0] [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0]

  return $ M.fromList [ ("player", vboSquare)
                      , ("circle", vboCircle)
                      , ("enemy", vboPentagon)
                      , ("room", vboRoom)
                      , ("ball", vboBall)
                      , ("triangle", vboTriangle)
                      , ("square", vboSquare)
                      , ("explosion", vboExplosition)
                      , ("smallExplosion", vboSmallExplosition)
                      , ("fullscreenQuad", vboFullscreenQuad)]

createFramebuffers :: IO (Map String Fbo)
createFramebuffers = do
  fboDefault <- fbo 1280 720

  return $ M.fromList [("default", fboDefault)]

createShaderPrograms :: IO (Map String ShaderProgramData)
createShaderPrograms = do
  defaultProgram <- newProgram "data/shaders/320/default.vert" "data/shaders/320/default.frag"
  passthruProgram <- newProgram "data/shaders/320/empty.vert" "data/shaders/320/blit.frag"

  return $ M.fromList [("default", defaultProgram), ("passthru", passthruProgram)]

renderActions :: [RenderAction]
renderActions = [render]

ioActions :: IOActions
ioActions = []

simulate :: Actors -> World -> (Actors, World)
simulate as w = runState state w
  where state = processTimesheet (stageOneTimesheet w) as >>= processInput >>= processActors >>= executeBulletCallback >>= filterBullets >>= produceBullets >>= processCollisions >>= movement

        player@(Player pn pp pq pv pa psr pst) = getPlayer as

        prepare :: Actors -> State World Actors
        prepare actors = return actors

        produceBullets :: Actors -> State World Actors
        produceBullets actors = do
          modify $ \world -> world { bullets = (bullets world) ++ shootOneBulletByPlayer (worldInput world) }
          return actors
            where shootOneBulletByPlayer :: Input -> Actors
                  shootOneBulletByPlayer input = shootOneBullet condition player
                    where (lb,rb) = inputMouseButtons input
                          condition = (lb || keySpace input || btnA input) && (playerShootingTimer player <= 0.0001)

        shootOneBullet :: Bool -> Actor -> Actors
        shootOneBullet b p@Player{} = [ bs | bs <- [Bullet "circle" Ally 10.0 pp initialVelocity zeroV passthru], b ]
          where initialVelocity = mulScalarVec (500 + (lengthVec $ playerVelocity p)) direction
                direction = rightV $ playerOrientation p

        shootOneBullet b e@Enemy{} = [ bs | bs <- [Bullet "triangle" Opponent 10.0 (enemyPosition e) initialVelocity zeroV passthru], b ]
          where initialVelocity = mulScalarVec (50 + (lengthVec $ enemyVelocity e)) direction
                direction = rightV $ enemyOrientation e -- right is our forward in 2d

        shootOneExplosiveBullet b e@Enemy{} = [ bs | bs <- [Bullet "triangle" Opponent 2.0 (enemyPosition e) initialVelocity zeroV explosive], b ]
          where initialVelocity = mulScalarVec (50 + (lengthVec $ enemyVelocity e)) direction
                direction = rightV $ enemyOrientation e -- right is our forward in 2d

        explosive :: Actor -> Actors
        explosive b = makeExplosion (bulletPosition b)

        makeExplosion :: Vector Float -> Actors
        makeExplosion p = map (\d -> Bullet "circle" Opponent 2.0 p (mulScalarVec 180 d) zeroV passthru) directions
          where directions = map circleVec [-pi, -(pi - (pi/3)) .. pi]

        processInput :: Actors -> State World Actors
        processInput as = do
          world <- get

          let jp = (inputJoystickAxisL (worldInput w))
              (ocpx:ocpy:rest) = worldCameraPosition world
              cp = [(Math.x jp) + ocpx, (Math.y jp) + ocpy, (-300.0), 1.0]
           in put $ world { worldCameraPosition = cp }

          return $ map (applyInput world) as

        applyInput :: World -> Actor -> Actor
        applyInput w player@Player{} = player { playerAcceleration = a }
                   where a = mulScalarVec 2200.0 (mulMV [x + jx,y + jy,0.0] (toMatrixQ (playerOrientation player)))
                         (x:y:rest) = inputAxisL (worldInput w)
                         [jx, jy] = take 2 (inputJoystickAxisL (worldInput w))

        applyInput w a = a

        executeBulletCallback :: Actors -> State World Actors
        executeBulletCallback actors = do
          world <- get

          let bs = concat $ map (\b -> if not (bulletAge b > 0) then (bc b) b else [b]) (bullets world)
              bc b = bulletCallback b
           in put $ world { bullets = bs }

          return actors

        filterBullets :: Actors -> State World Actors
        filterBullets actors = do
          modify $ \world -> world { bullets = filter f (bullets world) }
          return actors
            where f bullet@Bullet{} | bulletAge bullet > 0 = True
                                    | otherwise = False

        processCollisions :: Actors -> State World Actors
        processCollisions actors = do
          world <- get

          return $ concat $ map (eachActor (bullets world)) actors

            where -- f pl@(Player{}) b = (bulletTag b == Opponent) && (not $ collide ((Circle (playerPosition pl) 6.0), (Circle (bulletPosition b) 2.0)))
                  f a@(Enemy{}) b = (bulletTag b == Ally) && (not $ collide (mkCircle 12.0 a, mkCircle 2.0 b))
                  f _ _ = False

                  eachActor :: Actors -> Actor -> Actors
                  eachActor bs e@Enemy{} = if (not . null $ filter (f e) bs)
                                           then [tinyExplosion (position e), e { enemyAge = (enemyAge e) - 4.0}]
                                           else [e]
                  eachActor bs a = if (not . null $ filter (f a) bs) then [] else [a]

                  tinyExplosion p = Explosion "smallExplosion" p 1.5 8.0

        processActors :: Actors -> State World Actors
        processActors actors = do
          world <- get

          let as = filter isAlive $ map (behaviour world) actors
          --let as' = map (trajectory (worldTime world) (worldDt world)) as

          modify $ \w -> w { bullets = (bullets w) ++ (concat $ map (enemyShoot w) actors) }

          return as

            where timer w a = setShootingTimer (getShootingTimer a - (worldDt w)) a

                  reset p@Player{} | playerShootingTimer p < -0.0001 = p { playerShootingTimer = playerShootingRate p }
                                   | otherwise = p
                  reset e@Enemy{}  | enemyShootingTimer e < -0.0001 = e { enemyShootingTimer = enemyShootingRate e }
                                   | otherwise = e
                  reset a = a

                  age w e@Enemy{} = e { enemyAge = enemyAge e - (worldDt w)}
                  age w e@Explosion{} = e { explosionAge = explosionAge e - (5.0 * worldDt w)}
                  age w a = a

                  follow, flee :: World -> Actor -> Actor
                  follow w = followTarget w player
                  flee w = fleeTarget w player

                  behaviour :: World -> Actor -> Actor
                  behaviour w = follow w . age w . timer w . explode . reset

                  enemyShoot :: World -> Actor -> Actors
                  enemyShoot w e@Enemy{} =
                    case (enemyTag e) of
                      Boss1 -> shootOneExplosiveBullet condtion e
                      othewise -> shootOneBullet condtion e
                    where condtion = enemyShootingTimer e < -0.0001
                  enemyShoot w _ = []

        processEvents :: Events Int -> Actors -> State World Actors
        processEvents = undefined

        movement :: Actors -> State World Actors
        movement actors = do
          modify $ \world -> world { bullets = map (updateMovement (worldDt world)) (bullets world) }
          world <- get
          return $ map (updateMovement (worldDt world)) actors

        targetInRange :: (Float, Float) -> Actor -> Actor -> Bool
        targetInRange (rmin, rmax) target actor = inRange (rmin, rmax) distance
          where distance = distanceV (position actor) (position target)

        targetInRangeDefault :: Actor -> Actor -> Bool
        targetInRangeDefault = targetInRange (5.0, 10.0)

        -- animated background to simulate speeding through the world
        background :: Actors -> State World Actors
        background = undefined
