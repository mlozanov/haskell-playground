module Main where

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

import Ai

-- import TestFFI

type WorldState = State World World

newtype Simulation = Simulation (StateT World IO ())

main :: IO ()
main = setup 1280 720 "fbo test" setupAction renderActions simulate ioActions

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  let room = StaticActor "room" zeroV identityQ Type1

  positions <- mapM (\i -> rndSphereVec) [1.. 5000]

  modifyIORef actorsRef (\actors -> [] ++ (map (\p -> StaticActor "pentagon" p Math.identityQ Type1) (map (scaleVec 400.0) positions)))

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createFramebuffers

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map (* 140) room) (concat roomNormals)

  vboBall <- Vbo.fromList GL.Points ballVertices ballNormals

  vboPentagon <- Vbo.fromList GL.LineStrip (ngonVertices 14.0 5.0) (ngonNormals 5.0)

  vboFullscreenQuad <- Vbo.fromList GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0] [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0]

  return $ M.fromList [("room", vboRoom), ("ball", vboBall), ("pentagon", vboPentagon), ("fullscreenQuad", vboFullscreenQuad)]

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
simulate as w = (as,w)
