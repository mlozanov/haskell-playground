module Main where

import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL

import Data.IORef
import Data.Map as M hiding (map,filter,null)

import Backend

import Graphics

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
  let forest = StaticActor "forest" zeroV identityQ Type2

  modifyIORef actorsRef (\actors -> [room, forest] ++ actors)

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createFramebuffers

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

scale140 = (*) 140

forestVertices :: [GL.GLfloat]
forestVertices = concat vs
  where vs = [ toGLfloatList (sphereVec azimuth zenith) | azimuth <- [-pi, -pi/4.0 .. pi], zenith <- [-pi, -pi/4.0 .. pi]]


forestNormals :: [GL.GLfloat]
forestNormals = concat ns
  where ns = replicate 16 [0.0,0.0,1.0,0.0,0.0,1.0,0.0,0.0,1.0 :: GL.GLfloat]

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map scale140 room) (concat roomNormals)

  vboForest <- Vbo.fromList GL.Triangles (map scale140 forestVertices) forestNormals
  
  vboFullscreenQuad <- Vbo.fromList GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0] [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0]

  return $ M.fromList [ ("room", vboRoom)
                      , ("fullscreenQuad", vboFullscreenQuad)

                      , ("forest", vboForest)
                      ]

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
  where state :: State World Actors
        state = do w <- get 
                   return (map (updateMovement (worldDt w)) as)
