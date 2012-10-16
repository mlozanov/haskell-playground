{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO.Unsafe
import System.Random

import Control.Monad
import Control.Applicative

import qualified Graphics.Rendering.Cairo as C

data WorldData = WorldData [Double] deriving Show

data World = World { randomGenerator :: StdGen 
                   , numberOfIterations :: Int
                   , surface :: C.Surface
                   , worldData :: WorldData
                   } deriving Show

instance Show C.Surface where
  show s = "Cairo Surface"

setup :: World -> IO World
setup world = return world { randomGenerator = genNext, worldData = WorldData rs }
  where (gen, genNext) = split (randomGenerator world) 
        rs = take 100 $ randoms gen

animate :: World -> IO World
animate world = do C.renderWith (surface world) render
                   return world { randomGenerator = genNext, numberOfIterations = numberOfIterations world - 1 }
    where render :: C.Render ()
          render = do C.save
                      C.setOperator C.OperatorAdd
                      C.setAntialias C.AntialiasBest

                      --C.setSourceRGB 0.12 0.142 0.162
                      C.setSourceRGB 0.15 0.15 0.3

                      --mapM_ (\(x,y,z) -> C.moveTo x y >> C.rectangle (20*x+10*z) (20*y+10*z) 16 16 >> C.fill) grid
                      --mapM_ (\(x,y,z) -> C.moveTo x y >> C.arc (32*x+5-10*z) (32*y+5-10*z) 16 0 (2*3.14159) >> C.fill) grid
                      mapM_ (\(x,y,z) -> C.moveTo x y >> C.arc (32*x+2.2*z) (32*y+3.2*z) (24+0.6*n) 0 (2*3.14159) >> C.fill) grid

                      C.selectFontFace "PT Sans" C.FontSlantNormal C.FontWeightNormal

                      C.setFontSize 6
                      C.setSourceRGB 0 0 0

                      C.moveTo (4) (500 + 0.13 * r * n)
                      C.setOperator C.OperatorColorBurn

                      C.textPath "(c) 2012 mikhail lozanov"

                      --C.textPath "generative postcard"

                      --C.setOperator C.OperatorOver
                      --C.setSourceRGB 0 0 0 

                      --C.moveTo 0 412
                      --mapM_ (\(x,y) -> C.lineTo (5.12*x) (312+200*y)) (zip [1..100] rs)

                      C.fill
                      C.restore

          n = fromIntegral $ numberOfIterations world
          (genThis, genNext) = split $ randomGenerator world
          (r, _) = randomR (0, 1.0) genThis
          (WorldData rs) = worldData world
          rs' = take (17*17) (randoms genNext)
          grid :: [(Double, Double, Double)]
          grid = map (\((x,y),z) -> (x,y,z)) (zip [ (x,y) | x <- [0..16], y <- [0..16] ] rs')


assemble :: World -> (World -> IO World) -> (World -> IO World) -> IO World
assemble world setupCallback animateCallback = (setupCallback world) >>= animate' 
  where 
    animate' :: World -> IO World
    animate' w | numberOfIterations w > 0 = animateCallback w >>= animate'
               | otherwise = return w

genesis :: C.Surface -> World 
genesis surf = World (mkStdGen 1023) 3 surf (WorldData [])

main :: IO ()
main = C.withPDFSurface "GenerativePostCard.pdf" 512 512 mainLoop
  where mainLoop :: C.Surface -> IO ()
        mainLoop surf = assemble (genesis surf) setup animate >> return ()
