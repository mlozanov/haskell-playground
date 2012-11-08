{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Random
import System.IO.Unsafe

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

data World = World (Float,Float) [Float] [(Float, Float)]

nbCircles :: Int
nbCircles = 100

dt :: Float
dt = 1.0/60.0

circleColor = makeColor 0.3 0.5 0.3 1.0
lineColor = makeColor 0.3 0.5 0.3 0.5

k1 = makeColor8 43 25 59 255
k2 = makeColor8 95 133 65 255
k3 = makeColor8 140 167 56 255
k4 = makeColor8 169 204 94 255
k5 = makeColor8 189 228 133 255

main :: IO ()
main = simulate (InWindow "gloss playground" (1280, 720) (64,64)) k1 60 w render step
  where w = World (0.0,0.0) rs ps
        rs = unsafePerformIO $ rnds nbCircles
        ps = rndPositions nbCircles


render :: World -> Picture
render w = Pictures [circles, lines]
  where World (a,b) rs ps = w --unpack world state using language pattern match 
        cs = map oneCircle rs
        oneCircle r = Color k5 (ThickCircle (5 + 20 * r) (1 + 4 * r))
        circles = pictures $ map (\(c, (x,y)) -> translate x y c) (zip cs ps)
        lines = pictures $ map (\(p1,p2) -> Color k2 (Line [p1,p2])) ls
        ls = filter (inRange 60.0) [ (p,q) | p <- ps, q <- ps ]   -- (zip ps (tail ps))

inRange :: Float -> (Point, Point) -> Bool
inRange d (p1,p2) = sqrt (dx*dx+dy*dy) < d
  where (x1,y1) = p1
        (x2,y2) = p2
        (dx,dy) = (x2-x1,y2-y1)

step :: ViewPort -> Float -> World -> World
step vp t w = World (a', b') rs' ps'
  where World (a,b) rs ps = w
        rs' = map (\(x,v) -> x+(0.5-v)*dt) (zip rs (unsafePerformIO $ rnds nbCircles))
        ps' = map (\((x,y), (vx,vy)) -> (x+0.1*vx*dt, y+0.1*vy*dt)) (zip ps (rndPositions nbCircles))
        a' = a + t
        b' = b + t

rnds :: Int -> IO [Float]
rnds n = mapM (\_ -> randomIO) [1..n]

rndPositions :: Int -> [(Float, Float)]
rndPositions n = zip as bs
  where as = map (\x -> 800 * (0.5 - x)) rs1
        bs = map (\x -> 400 * (0.5 - x)) rs2
        rs1 = unsafePerformIO $ rnds n
        rs2 = unsafePerformIO $ rnds n


renderGrids :: World -> Picture
renderGrids = undefined