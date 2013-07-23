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

import Generator

-- import TestFFI

type WorldState = State World World

newtype Simulation = Simulation (StateT World IO ())


--- THE EXPERIMENT
scale140 = (*) 140

minusPiToPi scale = [(-pi), (-pi) - (-1.0*pi/scale) .. pi]

attachNormal v = v ++ (normalizeV . negateVec) v

treeTrunk :: [GL.GLfloat]
treeTrunk = (toGLfloatList . concat) tvs
  where vs = [ (sphereVec azimuth zenith) | zenith <- [-2.0*pi/16.0, pi], azimuth <- minusPiToPi 2.0]
        tvs = map attachNormal rvs
        rvs = map (rotateVQ q) vs
        q = fromAxisAngleQ 1.0 0.0 0.0 (degToRad 90.0)

treeTrunkIndices :: [GL.GLuint]
treeTrunkIndices = concat [ [c, i, i+1] | i <- [0..(c-2)] ]
  where c = toEnum vcount
        vcount = length treeTrunk `div` 12

randomizedSphereVertices :: [GL.GLfloat]
randomizedSphereVertices = toGLfloatList vs''
  where vs = [ sphereVec azimuth zenith | zenith <- minusPiToPi 16.0, azimuth <- minusPiToPi 16.0 ]
        tvs = map (rotateVQ q) rs'
        vs' = map triangleAtPosition tvs
        vs'' = concat $ map attachNormal vs'
        q = fromAxisAngleQ 1.0 0.0 0.0 (degToRad 30.0)

        triangleAtPosition position = position -- concat $ map (addVec position) (ngonVerticesVec 0.2 3.0)

        rs = take (length vs) $ randoms (mkStdGen 1023)
        rs' = map (\(v,f) -> mulScalarAddVec (f*0.1) v) (zip vs rs)


randomizedSphereIndices :: [GL.GLuint]
randomizedSphereIndices = concat [ [i, i+1, i+33, i, i+32, i+33] | i <- [0 .. (c-32)] ]
  where c = toEnum vcount
        vcount = length randomizedSphereVertices `div` 12

--RandomizedSphereNormals :: [GL.GLfloat]
--RandomizedSphereNormals = concat ns
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
  let randomizedSphere = StaticActor "randomizedsphere" zeroV identityQ Type2
  let trunk = StaticActor "treetrunk" zeroV identityQ Type2

  modifyIORef actorsRef (\actors -> [trunk] ++ actors)

  renderState <- readIORef renderStateRef

  shaders <- createShaderPrograms
  objects <- createGeometryObjects
  renderTargets <- createFramebuffers

  writeIORef renderStateRef (renderState { shaderProgramsMap = shaders, vboMap = objects, fboMap = renderTargets } )

createGeometryObjects :: IO (Map String Vbo)
createGeometryObjects = do
  vboRoom <- Vbo.fromList GL.Triangles (map scale140 room) (concat roomNormals)

  vboRandomizedSphere <- Vbo.fromList' GL.Triangles (map scale140 randomizedSphereVertices) randomizedSphereIndices

  vboFullscreenQuad <- Vbo.fromList' GL.TriangleStrip [0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0] [0, 1, 3, 2]

  vboTrunk <- Vbo.fromList' GL.Triangles (map scale140 treeTrunk) treeTrunkIndices

  return $ M.fromList [ ("room", vboRoom)
                      , ("fullscreenQuad", vboFullscreenQuad)

                      , ("randomizedsphere", vboRandomizedSphere)
                      , ("treetrunk", vboTrunk)
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
