module Camera where

import Control.Monad.State

import Actor

data Camera = EmptyCamera
            | Camera [Double]
              deriving (Eq, Ord, Show)

type Cameras = [Camera]

cameraFixed :: Camera -> State Actors Camera
cameraFixed camera = return camera

cameraOrbit :: Camera -> State Actors Camera
cameraOrbit camera = return camera

cameraDamp :: Camera -> State Actors Camera
cameraDamp camera = return camera

cameraPan :: Camera -> State Actors Camera
cameraPan camera = return camera

cameraFrameActors :: Camera -> State Actors Camera
cameraFrameActors camera = return camera

simpleFraming camera = cameraOrbit camera >>= cameraDamp >>= cameraFrameActors

