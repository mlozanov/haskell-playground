module Camera where

import Control.Monad.State

import Math
import Actor

data Camera = EmptyCamera
            | Camera { transform :: Matrix Float
                     , position :: Vector Float
                     , orientation :: Quaternion Float
                     , up :: Vector Float
                     , direction :: Vector Float
                     , fov :: Float }
            | TargetCamera { transform :: Matrix Float
                           , position :: Vector Float
                           , orientation :: Quaternion Float
                           , up :: Vector Float
                           , targetPosition :: Vector Float
                           , fov :: Float }
              deriving (Show)

type Cameras = [Camera]

cameraFixed :: Camera -> State Actors Camera
cameraFixed camera = return camera

cameraOrbit :: Camera -> State Actors Camera
cameraOrbit camera = return camera

cameraDamp :: Camera -> State Actors Camera
cameraDamp camera = return camera

cameraPan :: Camera -> State Actors Camera
cameraPan EmptyCamera = return EmptyCamera
cameraPan c = return c

cameraFrameActors :: Camera -> State Actors Camera
cameraFrameActors camera = return camera

cameraFinalize :: Camera -> State Actors Camera
cameraFinalize EmptyCamera = return EmptyCamera
cameraFinalize c = return $ c { transform = r `mulMM` translate (x p) (y p) (z p) }
    where p = position c
          r = toMatrixQ $ orientation c


-- compositions
simpleFraming camera = cameraOrbit camera >>= cameraDamp >>= cameraFrameActors >>= cameraFinalize

