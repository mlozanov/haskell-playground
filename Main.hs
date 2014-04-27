module Main where

import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL

import Data.IORef
import Data.Map as M hiding (map,filter,null)

import System.Random

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

import Geometry

-- import TestFFI

type WorldState = State World World

newtype Simulation = Simulation (StateT World IO ())


main :: IO ()
main = setup 1280 720 "sharpshooter \\ procedural" setupAction renderActions simulate ioActions

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  let room = StaticActor "room" zeroV identityQ Type1
  let randomizedSphere = StaticActor "randomizedsphere" zeroV identityQ Type2
  let trunk = StaticActor "treetrunk" zeroV identityQ Type1
  let leaf = StaticActor "treeleaf" zeroV identityQ Type1
  let rndCyl = StaticActor "cyl" zeroV identityQ Type2

  let f :: Actor -> [Float] -> Actor
      f = \a [x,y,z] ->
          setPosition [x,y,z] a

  let positionTrunk = f trunk
  let positionLeaf = f leaf

  let ts :: Actors
      ts = map positionTrunk [ [(x+80),(y+80), (-100.0)] | x <- [-600.0, -400.0 .. 600.0], y <- [-600.0, -400.0 .. 600.0] ]

  let ls :: Actors
      ls = map positionLeaf [ [(x+80),(y+80), (-100.0)] | x <- [-600.0, -400.0 .. 600.0], y <- [-600.0, -400.0 .. 600.0] ]

  let as :: Actors
      as = [(setPosition [-250.0, 0.0, -150.0] rndCyl), (setPosition [0.0, 100.0, -100.0] leaf), (setPosition [250.0, 0.0, -150.0] randomizedSphere)] 
  modifyIORef actorsRef (\actors ->  as ++ actors)

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createFramebuffers

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map scale140 room) (concat roomNormals)
  vboFullscreenQuad <- Vbo.fromList' GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0] [0, 1, 3, 2]

  vboTrunk <- Vbo.fromPairList' GL.Triangles theTrunk
  vboLeaf <- Vbo.fromPairList' GL.Triangles theLeaf

  vboRandomizedSphere <- Vbo.fromPairList' GL.Triangles (vertices s1, indices s1)
  vboRndCyl <- Vbo.fromPairList' GL.Triangles (vertices c1, indices c1)

  return $ M.fromList [ ("room", vboRoom)
                      , ("fullscreenQuad", vboFullscreenQuad)

                      , ("randomizedsphere", vboRandomizedSphere)
                      , ("treetrunk", vboTrunk)
                      , ("treeleaf", vboLeaf)
                      , ("cyl", vboRndCyl)
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
