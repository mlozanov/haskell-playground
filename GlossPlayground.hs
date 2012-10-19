{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Random
import System.IO.Unsafe

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

data World = World (Float,Float) [Float] [(Float, Float)]

nbCircles :: Int
nbCircles = 100

main :: IO ()
main = simulate (InWindow "gloss playground" (1280, 720) (64,64)) (greyN 0.2) 60 w render step
  where w = World (0.0,0.0) rs ps
        rs = unsafePerformIO $ rnds nbCircles
        ps = rndPositions nbCircles

render :: World -> Picture
render w = Pictures [circles, lines]
  where World (a,b) rs ps = w --unpack world state using language pattern match 
        cs = map oneCircle rs
        oneCircle r = Color green (ThickCircle (20* r) (4 * r))
        circles = pictures $ map (\(c, (x,y)) -> translate x y c) (zip cs ps)
        lines = pictures $ map (\(p1,p2) -> Color red (Line [p1,p2])) ls
        ls = filter (inRange 330.0) (zip ps (tail ps))

inRange :: Float -> (Point, Point) -> Bool
inRange d (p1,p2) = sqrt (dx*dx+dy*dy) < d
  where (x1,y1) = p1
        (x2,y2) = p2
        (dx,dy) = (x2-x1,y2-y1)

step :: ViewPort -> Float -> World -> World
step vp t w = World (a', b') rs' ps'
  where World (a,b) rs ps = w
        rs' = map (\(x,v) -> x+(0.5-v)*0.01666667) (zip rs (unsafePerformIO $ rnds nbCircles))
        ps' = map (\((x,y), (vx,vy)) -> (x+vx*0.01, y+vy*0.01666667)) (zip ps (rndPositions nbCircles))
        a' = a + t
        b' = b + t

rnds :: Int -> IO [Float]
rnds n = mapM (\_ -> randomIO) [1..n]

rndPositions :: Int -> [(Float, Float)]
rndPositions n = zip as bs
  where as = map (\x -> 1000 * (0.5 - x)) (unsafePerformIO $ rnds n)
        bs = map (\x -> 500 * (0.5 - x)) (unsafePerformIO $ rnds n)

