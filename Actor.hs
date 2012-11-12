module Actor where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Objects as O

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
                   , enemyPosition :: !(Vector Float )
                   , enemyOrientation :: !(Quaternion Float)
                   , enemyVelocity :: !(Vector Float)
                   , enemyAcceleration :: !(Vector Float)
                   }

type Actors = [Actor]

newPlayer :: Actor
newPlayer = Player "player" zeroV identityQ zeroV ([0.0, 0.0, 0.0, 0.0])

newEnemy :: Actor
newEnemy = Enemy "enemy" zeroV identityQ zeroV ([400.0, 100.0, 0.0, 0.0])

newEnemy1 :: Actor
newEnemy1 = Enemy "enemy" zeroV identityQ zeroV ([0.0, 200.0, 0.0, 0.0])

newEnemy2 :: Actor
newEnemy2 = Enemy "enemy" zeroV identityQ zeroV ([200.0, 400.0, 0.0, 0.0])

