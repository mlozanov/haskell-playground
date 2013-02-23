module World where

import System.Random

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

