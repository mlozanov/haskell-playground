module World where

import System.Random
import Data.IORef

import Input
import Camera
import Actor

data Statistics = Statistics { statShootAccuracy :: Float
                             , statAverageTimeToShootEnemy :: Float
                             } deriving Show

data World = World { worldTime :: !Int
                   , worldInput :: !Input 
                   , worldDt :: !Float
                   , cameras :: !Cameras
                   , bullets :: !Actors
                   , gen :: !StdGen
                   } deriving Show

worldInputButtons :: World -> [Bool]
worldInputButtons world = inputButtons (worldInput world)

worldInputMouseButtons :: World -> (Bool,Bool)
worldInputMouseButtons world = inputMouseButtons (worldInput world)


randomFloatUnit :: World -> Float
randomFloatUnit world = 2 * (0.5 - r)
    where r = fst $ random (gen world)

randomFloatR :: World -> Float -> Float -> Float
randomFloatR world a b = fst $ randomR (a,b) (gen world)

--- debugging

debugInput :: IORef World -> IO ()
debugInput worldRef = do world <- readIORef worldRef
                         print (worldInput world)


debugWorld :: IORef World -> IO ()
debugWorld worldRef = readIORef worldRef >>= print

