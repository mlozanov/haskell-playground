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
scale80 = (*) 80

minusPiToPi scale = [(-pi), (-pi) - (-1.0*pi/scale) .. pi]

attachNormal v = v ++ (normalizeV) v

concatGL = toGLfloatList . concat

treeTrunk :: [GL.GLfloat]
treeTrunk = f vs
  where vs = [ (sphereVec azimuth zenith) | zenith <- [-2.0*pi/16.0, pi], azimuth <- minusPiToPi 2.0]
        f = concatGL . map attachNormal . map (rotateXQ 90.0) . map (mulVec [80.0, 80.0, 140.0])

treeTrunkIndices :: [GL.GLuint]
treeTrunkIndices = concat [ [c, i, i+1] | i <- [0..c] ]
  where c = toEnum $ length treeTrunk `div` 12

treeLeaf :: [GL.GLfloat]
treeLeaf = f vs
  where vs = [ sphereVec azimuth zenith | zenith <- [-pi*0.5, pi*0.5], azimuth <- minusPiToPi 3.0 ]
        f = concatGL . 
            map attachNormal . 
            map (rotateXQ 60.0) . 
            map (mulVec [50.0, 50.0, 50.0]) . 
            map (translateYV 1.0)

treeLeafIndices :: [GL.GLuint]
treeLeafIndices = concat [ [c, i, i+1] | i <- [0..c] ]
  where c = toEnum $ length treeLeaf `div` 12

theLeaf = (treeLeaf, treeLeafIndices)
theTrunk = (treeTrunk, treeTrunkIndices)

randomizedSphereVertices :: [GL.GLfloat]
randomizedSphereVertices = f rs
  where vs = [ sphereVec azimuth zenith | azimuth <- minusPiToPi 32.0, zenith <- minusPiToPi 32.0 ]
        f = concatGL . map attachNormal . map (rotateXQ 0.0) . map (\(v,f) -> mulScalarAddVec (f*0.2) v) . zip vs . take (length vs)
        rs = randoms (mkStdGen 1023)


randomizedSphereIndices :: [GL.GLuint]
randomizedSphereIndices = concat [ [i, i+1, i+33, i, i+32, i+33] | i <- [0 .. (c-32)] ]
  where c = toEnum $ length randomizedSphereVertices `div` 12


randomizedSphereOfPointsV :: [GL.GLfloat]
randomizedSphereOfPointsV = vs
  where vs = []

randomizedCylinderV :: [GL.GLfloat]
randomizedCylinderV = f (fst mkRS)
  where f = concatGL . map attachNormal . map (rotateXQ 90.0) . map (mulVec [100.0, 100.0, 100.0])

        rs :: StdGen -> [Vector Float] -> Int -> ([Vector Float], StdGen)
        rs g vs 0 = (v:vs, g')
          where (v, g') = rndCylinderV g
        rs g vs i = rs g' (v:vs) (i-1)
          where (v, g') = rndCylinderV g

        mkRS = rs (mkStdGen 8347) [] 2000

randomizedCylinderIndices :: [GL.GLuint]
randomizedCylinderIndices = [0 .. c]
  where c = toEnum $ length randomizedCylinderV `div` 6

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
main = setup 1280 720 "sharpshooter \\ procedural" setupAction renderActions simulate ioActions

-- setup and render actinos are monadic to work with IO
setupAction :: SetupAction
setupAction worldRef actorsRef renderStateRef = do
  let room = StaticActor "room" zeroV identityQ Type1
  let randomizedSphere = StaticActor "randomizedsphere" zeroV identityQ Type2
  let trunk = StaticActor "treetrunk" zeroV identityQ Type2
  let leaf = StaticActor "treeleaf" zeroV identityQ Type2
  let rndCyl = StaticActor "cyl" zeroV identityQ Type2

  modifyIORef actorsRef (\actors -> [rndCyl] ++ actors) -- , trunk, leaf, (setPosition [250.0, 0.0, -250.0] randomizedSphere)

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

  vboTrunk <- Vbo.fromPairList' GL.Triangles theTrunk
  vboLeaf <- Vbo.fromPairList' GL.Triangles theLeaf

  vboRndCyl <- Vbo.fromPairList' GL.Points (randomizedCylinderV, randomizedCylinderIndices)

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
