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

import Generator

-- import TestFFI

type WorldState = State World World

newtype Simulation = Simulation (StateT World IO ())


--- THE EXPERIMENT
scale140 = (*) 140

minusPiToPi scale = [(-pi) - (-pi/scale), (-pi) - (-2.0*pi/scale) .. pi]

treeTrunk :: [GL.GLfloat]
treeTrunk = (toGLfloatList . concat) [ sphereVec azimuth zenith | zenith <- [-2.0*pi/3.0, pi], azimuth <- minusPiToPi 16.0]

forestVertices :: [GL.GLfloat]
forestVertices = toGLfloatList vs''
  where vs = [ sphereVec azimuth zenith | zenith <- minusPiToPi 6.0, azimuth <- minusPiToPi 6.0 ]
        tvs = map (rotateVQ q) vs
        vs' = map triangleAtPosition tvs
        vs'' = concat $ map attachNormal vs'
        q = fromAxisAngleQ 1.0 0.0 0.0 (degToRad 45.0)

        triangleAtPosition position = position -- concat $ map (addVec position) (ngonVerticesVec 0.2 3.0)

        attachNormal v = v ++ (normalizeV . negateVec) v

forestIndices :: [GL.GLuint]
forestIndices = concat [ [i, i+1, i+12, i, i+12, i+11] | i <- [0 .. (c-18)] ]
  where c = toEnum vcount
        vcount = length forestVertices `div` 12

--forestNormals :: [GL.GLfloat]
--forestNormals = concat ns
--  where ns = replicate 2048 [0.0,0.0,1.0,0.0,0.0,1.0,0.0,0.0,1.0 :: GL.GLfloat]

normalFromPolygon :: [Vector Float] -> Vector Float 
normalFromPolygon (p:q:ps) = c
  where a = subVec p q
        b = subVec q (head ps)
        c = crossVec a b

--- THE EXPERIMENT

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions simulate ioActions

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  let room = StaticActor "room" zeroV identityQ Type1
  let forest = StaticActor "forest" zeroV identityQ Type2

  modifyIORef actorsRef (\actors -> [forest] ++ actors)

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createFramebuffers

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map scale140 room) (concat roomNormals)

  --vboForest <- Vbo.fromList GL.Triangles (map scale140 forestVertices) forestNormals
  --vboFullscreenQuad <- Vbo.fromList GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0] [0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0]

  vboForest <- Vbo.fromList' GL.Triangles (map scale140 forestVertices) forestIndices

  vboFullscreenQuad <- Vbo.fromList' GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0] [0, 1, 3, 2]

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
