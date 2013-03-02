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

           | Player { playerName :: String
                    , playerPosition :: (Vector Float)
                    , playerOrientation :: (Quaternion Float)
                    , playerVelocity :: (Vector Float)
                    , playerAcceleration :: (Vector Float)

                    , playerShootingRate :: Float
                    , playerShootingTimer :: Float
                    }

           | Enemy { enemyName :: String
                   , enemyPosition :: (Vector Float)
                   , enemyOrientation :: (Quaternion Float)
                   , enemyVelocity :: (Vector Float)
                   , enemyAcceleration :: (Vector Float)

                   , enemyShootingRate :: Float
                   , enemyShootingPattern :: ShootingPattern

                   , enemyShootingTimer :: Float
                   }

           | StaticActor { staticActorName :: String 
                         , staticActorPosition :: Vector Float
                         , staticActorOrientation :: Quaternion Float
                         }

           | Bullet { bulletName :: String
                    , bulletAge :: Float
                    , bulletPosition :: (Vector Float)
                    , bulletVelocity :: (Vector Float)
                    , bulletAcceleration :: (Vector Float)
                    , bulletCallback :: (Actor -> Actors)
                    }

           | Rocket { rocketName :: String
                    , rocketPosition :: (Vector Float)
                    }

           | Explosion { explosionName :: String 
                       , explosionPosition :: (Vector Float)
                       , explosionAge :: Float
                       , explosionPower :: Float
                       }

type Actors = [Actor]

newPlayer :: Actor
newPlayer = Player "player" zeroV identityQ zeroV zeroV 0.1 0.0

newEnemy :: Actor
newEnemy = Enemy "enemy" zeroV identityQ zeroV zeroV 0.5 Single 0.0

defaultEnemy :: Vector Float -> Actor
defaultEnemy p = newEnemy { enemyPosition = mulScalarVec 120.0 p
                          , enemyVelocity = mulScalarVec 15.0 p } 

newBullet :: Actor
newBullet = Bullet "bullet" 1.0 zeroV zeroV zeroV passthru

newExplosion :: Vector Float -> Actor
newExplosion p = Explosion "explosion" p 1.5 8.0

passthru :: Actor -> Actors
passthru a = [a]

instance Show Actor where
  show p@Player{} = "player:" ++ show (playerShootingTimer p) ++ "\n"
  show b@Bullet{} = "bullet:" ++ show (bulletAge b) ++ "\n"
  show e@Enemy{} = "enemy: shooting timer: " ++ show (enemyShootingTimer e) ++ "\n"
  show sa@StaticActor{} = "static actor:" ++ (staticActorName sa) ++ "\n"
  show a = show "actor" ++ "\n"
