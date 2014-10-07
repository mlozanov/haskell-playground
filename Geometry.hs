module Geometry where

import Control.Monad
import Control.Monad.State.Strict

import System.Random

import qualified Graphics.Rendering.OpenGL as GL

import Math
import Graphics

data Geometry = GeometrySphere Float
              | GeometryCylinder Float Float
              | GeometryNgon Int Float
  deriving Show

data Direction = AxisX
               | AxisY
               | AxisZ
  deriving Show

class HasVertices a where
  vertices :: a -> [GL.GLfloat]

class HasIndices a where
  indices :: a -> [GL.GLuint]

class GeomtryCommands a where
  extrude :: a -> Direction -> Float -> a

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

  --vertices (GeometryNgon resolution radius) = f vs
  --  where f = concatGL .


instance HasIndices Geometry where
  indices g@(GeometrySphere radius) = concat [ [i, i+1, i+17, i, i+16, i+17] | i <- [0 .. c] ]
    where c = toEnum $ length (vertices g) `div` 12

  indices g@(GeometryCylinder radius height) = [0 .. c]
    where c = toEnum $ length (vertices g) `div` 6


s1 = GeometrySphere 140.0
c1 = GeometryCylinder 100.0 120.0

--- THE EXPERIMENT
scale140, scale80 :: GL.GLfloat -> GL.GLfloat
scale140 = (*) (140.0 :: GL.GLfloat)
scale80 = (*) (80.0 :: GL.GLfloat)

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
