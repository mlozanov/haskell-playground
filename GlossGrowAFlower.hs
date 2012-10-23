{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Random
import System.IO.Unsafe

import Control.Monad

import Data.Tree

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Simulate

data CircleState = CS Float

data World = World CircleState [Float] [(Float, Float)]

subV :: Vector -> Vector -> Vector
subV (x1,y1) (x2,y2) = (x2-x1,y2-y1)

k1 = makeColor8 242 198 95 255
k2 = makeColor8 51 76 122 255
k3 = makeColor8 80 163 133 255
k4 = makeColor8 255 238 160 255
k5 = makeColor8 214 128 51 255

ks = [k1,k2,k3,k4,k5]

inRange :: Float -> (Point, Point) -> Bool
inRange d (p1,p2) = magV v < d
  where v = subV p1 p2

main :: IO ()
main = simulate (InWindow "gloss :: grow a flower" (1280, 720) (16,16)) (greyN 0.18) 60 w renderGrowAFlower simulateGrowAFlow
  where w = World (CS 1.0) rs ps
        rs = unsafePerformIO $ rnds 50
        ps = unsafePerformIO $ rndPositions 50

rnds :: Int -> IO [Float]
rnds n = mapM (\_ -> randomIO) [1..n]

rndPositions :: Int -> IO [(Float, Float)]
rndPositions n = liftM2 zip as bs -- promote zip to IO Monad
  where as = rnds n
        bs = rnds n

-- grow a flower
renderGrowAFlower :: World -> Picture
renderGrowAFlower w = pictures [color k2 (Circle (200*(sin r))), Translate (-640) (360-32) colourPalette, (tree 5 [])]
  where World (CS r) rs ps = w

simulateGrowAFlow :: ViewPort -> Float -> World -> World
simulateGrowAFlow vp t w = World (CS (v+t)) rs ps
  where World (CS v) rs ps = w

colourPalette :: Picture
colourPalette = pictures $ map oneSquare (zip [0.0..4.0] ks)
  where square k = Color k $ Polygon [(0,0), (32,0), (32,32), (0,32)]
        oneSquare (offset, k) = Translate (32*offset) 0 (square k)

branch :: Float -> Picture
branch r = scale 64 64 $ pictures [ Color k3 (Line [(0,0), (0,1.0)]), Translate 0 1.0 (Color k4 (ThickCircle (r*0.125) (r*0.25))) ]

tree :: Int -> [Picture] -> Picture
tree n ps | n == 0 = pictures ps
          | otherwise = tree (n-1) ([p] ++ ps)
  where p = Rotate (70*(fromIntegral n)) (branch 1.0)