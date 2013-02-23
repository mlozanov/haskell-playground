module World where

import System.Random
import Data.IORef

import Input
import Camera
import Actor

data World = World { worldTime :: !Float
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

debugInput :: IORef World -> IO ()
debugInput worldRef = do world <- readIORef worldRef
                         print (worldInput world)


debugWorld :: IORef World -> IO ()
debugWorld worldRef = readIORef worldRef >>= print