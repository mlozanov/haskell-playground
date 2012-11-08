module Actor where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Objects as O

import Math
import Graphics

data Actor = SimpleActor
           | Actor [Double]
           | Player { playerName :: String
                    , playerPosition :: Vector Float 
                    , playerOrientation :: Quaternion Float
                    , playerVelocity :: Vector Float
                    , playerAcceleration :: Vector Float
                    }
           | Enemy { enemyName :: String
                   , enemyPosition :: Vector Float 
                   , enemyOrientation :: Quaternion Float
                   , enemyVelocity :: Vector Float
                   , enemyAcceleration :: Vector Float
                   }
             deriving (Eq, Ord, Show)

type Actors = [Actor]



