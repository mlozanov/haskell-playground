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

  modifyIORef actorsRef (\actors -> [room])

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createRenderTargets

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map (* 140) room) (concat roomNormals)

  vboFullscreenQuad <- Vbo.fromList GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0] [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0]

  return $ M.fromList [("room", vboRoom), ("fullscreenQuad", vboFullscreenQuad)]

createShaderPrograms :: IO (Map String ShaderProgramData)
createShaderPrograms = do
  defaultProgram <- newProgram "data/shaders/320/default.vert" "data/shaders/320/default.frag" 
  passthruProgram <- newProgram "data/shaders/320/empty.vert" "data/shaders/320/blit.frag"

  return $ M.fromList [("default", defaultProgram), ("passthru", passthruProgram)]

createRenderTargets :: IO (Map String Fbo)
createRenderTargets = do
  return $ M.fromList []  

renderActions :: [RenderAction]
renderActions = [render]

ioActions :: IOActions
ioActions = []

simulate :: Actors -> World -> (Actors, World)
simulate as w = (as,w)
