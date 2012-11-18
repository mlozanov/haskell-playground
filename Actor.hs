module Actor where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Objects as O

import Data.Map

import Math
import Graphics

data Actor = SimpleActor

           | Player { playerName :: !String
                    , playerPosition :: !(Vector Float)
                    , playerOrientation :: !(Quaternion Float)
                    , playerVelocity :: !(Vector Float)
                    , playerAcceleration :: !(Vector Float)
                    }

           | Enemy { enemyName :: !String
                   , enemyPosition :: !(Vector Float)
                   , enemyOrientation :: !(Quaternion Float)
                   , enemyVelocity :: !(Vector Float)
                   , enemyAcceleration :: !(Vector Float)
                   }

           | StaticActor { staticActorName :: String 
                         , staticActorPosition :: Vector Float
                         , staticActorOrientation :: Quaternion Float
                         }

           | Bullet { bulletName :: !String
                    , bulletPosition :: !(Vector Float)
                    , bulletVelocity :: !(Vector Float)
                    , bulletAcceleration :: !(Vector Float)
                    }

           | Rocket { rocketName :: !String
                    , rocketPosition :: !(Vector Float)
                    }

            deriving Show

type Actors = [Actor]

newPlayer :: Actor
newPlayer = Player "player" zeroV identityQ zeroV zeroV

newEnemy :: Actor
newEnemy = Enemy "enemy" zeroV identityQ zeroV zeroV

