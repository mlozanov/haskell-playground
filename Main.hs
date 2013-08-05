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

data Geometry = GeometrySphere Float
              | GeometryCylinder Float Float
  deriving Show

class HasVertices a where
  vertices :: a -> [GL.GLfloat]

class HasIndices a where
  indices :: a -> [GL.GLuint]

instance HasVertices Geometry where
  vertices (GeometrySphere radius) = f rs
    where vs = [ sphereVec azimuth zenith | zenith <- minusPiToPi 16.0, azimuth <- minusPiToPi 8.0 ]
          f = concatGL . map attachNormal . map (mulScalarVec radius) . map (rotateXQ 0.0) . map (\(v,f) -> mulScalarAddVec (f*0.0) v) . zip vs . take (length vs)
          rs = randoms (mkStdGen 1023)

  vertices (GeometryCylinder radius height) = f (fst mkRS)
    where f = concatGL . map attachNormal . map (rotateXQ 90.0) . map (mulVec [radius, radius, height])

          -- rs :: StdGen -> [Vector Float] -> Int -> ([Vector Float], StdGen)
          rs g vs 0 = (v:vs, g')
            where (v, g') = rndInsideCylinderV g
          rs g vs i = rs g' (v:vs) (i-1)
            where (v, g') = rndInsideCylinderV g

          mkRS = rs (mkStdGen 8347) [] 2000


instance HasIndices Geometry where
  indices g@(GeometrySphere radius) = concat [ [i, i+1, i+17, i, i+16, i+17] | i <- [0 .. c] ]
    where c = toEnum $ length (vertices g) `div` 12

  indices g@(GeometryCylinder radius height) = [0 .. c]
    where c = toEnum $ length (vertices g) `div` 6


s1 = GeometrySphere 140.0
c1 = GeometryCylinder 100.0 120.0

--- THE EXPERIMENT
scale140 = (*) 140
scale80 = (*) 80

minusPiToPi scale = [(-pi), (-pi) - (-1.0*pi/scale) .. pi]

attachNormal v = v ++ (normalizeV) v

concatGL = toGLfloatList . concat

treeTrunk :: [GL.GLfloat]
treeTrunk = f vs
  where vs = [ (sphereVec azimuth zenith) | zenith <- [-2.0*pi/16.0, pi], azimuth <- minusPiToPi 2.0]
        f = concatGL . map attachNormal . map (rotateXQ (90+90.0)) . map (mulVec [60.0, 60.0, 240.0])

treeTrunkIndices :: GL.GLuint -> [GL.GLuint]
treeTrunkIndices start = concat [ [c, i, i+1] | i <- [start .. c] ]
  where c = toEnum $ length treeTrunk `div` 12

treeLeaf :: [GL.GLfloat]
treeLeaf = f vs
  where vs = [ sphereVec azimuth zenith | zenith <- [-pi*0.5, pi*0.5], azimuth <- minusPiToPi 3.0 ]
        f = concatGL . 
            map attachNormal . 
            map (rotateXQ (90+60.0)) . 
            map (mulVec [80.0, 80.0, 80.0]) . 
            map (translateYV 1.0)

treeLeafIndices :: GL.GLuint -> [GL.GLuint]
treeLeafIndices start = concat [ [c, i, i+1] | i <- [start .. c] ]
  where c = toEnum $ length treeLeaf `div` 12

theLeaf = (treeLeaf, treeLeafIndices 0)
theTrunk = (treeTrunk, treeTrunkIndices 0)

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

  --- [(setPosition [-250.0, 0.0, -150.0] rndCyl), (setPosition [0.0, 100.0, -100.0] leaf), (setPosition [250.0, 0.0, -150.0] randomizedSphere)] ++ 
  modifyIORef actorsRef (\actors -> ts ++ ls ++ actors)

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
