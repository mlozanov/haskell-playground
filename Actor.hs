module Actor where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Objects as O

import Data.Map

import Math
import Graphics

class Physical a where
  updateMovement :: Float -> a -> a

class Tickable a where
  tick :: Float -> a -> a

data ShootingPattern = Single
                     | Double
                     | Cross
                     | Fontain


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

                   , enemyShootingRate :: !Float
                   , enemyShootingPattern :: !ShootingPattern
                   }

           | StaticActor { staticActorName :: String 
                         , staticActorPosition :: Vector Float
                         , staticActorOrientation :: Quaternion Float
                         }

           | Bullet { bulletName :: !String
                    , bulletAge :: !Float
                    , bulletPosition :: !(Vector Float)
                    , bulletVelocity :: !(Vector Float)
                    , bulletAcceleration :: !(Vector Float)
                    , bulletCallback :: (Actor -> Actors)
                    }

           | Rocket { rocketName :: !String
                    , rocketPosition :: !(Vector Float)
                    }

           | Explosion { explosionName :: !String 
                       , explosionPosition :: !(Vector Float)
                       , explosionAge :: !Float
                       , explosionPower :: !Float
                       }

type Actors = [Actor]

newPlayer :: Actor
newPlayer = Player "player" zeroV identityQ zeroV zeroV

newEnemy :: Actor
newEnemy = Enemy "enemy" zeroV identityQ zeroV zeroV 1.0 Single

defaultEnemy :: Vector Float -> Actor
defaultEnemy p = Enemy "enemy" (mulScalarVec 120.0 p) identityQ (mulScalarVec 15.0 p) zeroV 1.0 Single

newBullet :: Actor
newBullet = Bullet "bullet" 1.0 zeroV zeroV zeroV passthru

newExplosion :: Vector Float -> Actor
newExplosion p = Explosion "explosion" p 1.5 8.0

passthru :: Actor -> Actors
passthru a = [a]

instance Show Actor where
  show (Bullet n age p v a f) = "bullet:" ++ show age
  show a = show "actor"
