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

data ShootingPattern = Single | Double | Cross | Fontain deriving (Show)

type ShootingString = [ShootingPattern]

data BulletTag = Ally | Opponent deriving (Eq, Ord)

data ActorTag = Type1 | Type2 | Boss1 | Boss2 deriving (Eq, Ord, Show)

data ActorAI = ActorAI { actorAiTag :: ActorTag
                       , actorAiShootingString :: ShootingString 
                       }
                       deriving (Show)

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
                   , enemyAge :: Float

                   , enemyPosition :: (Vector Float)
                   , enemyOrientation :: (Quaternion Float)
                   , enemyOmega :: (Vector Float)
                   , enemyVelocity :: (Vector Float)
                   , enemyAcceleration :: (Vector Float)

                   , enemyShootingRate :: Float
                   , enemyShootingPattern :: ShootingPattern

                   , enemyShootingTimer :: Float

                   , enemyTag :: ActorTag
                   }

           | StaticActor { staticActorName :: String 
                         , staticActorPosition :: Vector Float
                         , staticActorOrientation :: Quaternion Float

                         , tag :: ActorTag
                         }

           | Bullet { bulletName :: String
                    , bulletTag :: BulletTag
                    , bulletAge :: Float
                    , bulletPosition :: (Vector Float)
                    , bulletVelocity :: (Vector Float)
                    , bulletAcceleration :: (Vector Float)
                    , bulletCallback :: (Actor -> Actors)
                    }

           | Rocket { rocketName :: String
                    , rocketPosition :: (Vector Float)
                    , rocketVelocity :: (Vector Float)
                    , rocketAcceleration :: (Vector Float)
                    , rocketCallback :: (Actor -> Actors)
                    }

           | Explosion { explosionName :: String 
                       , explosionPosition :: (Vector Float)
                       , explosionAge :: Float
                       , explosionPower :: Float
                       }

type Actors = [Actor]

newPlayer :: Actor
newPlayer = Player "player" zeroV identityQ zeroV zeroV 0.15 0.0

newEnemy :: Actor
newEnemy = Enemy "enemy" 20.0 zeroV identityQ zeroV zeroV zeroV 0.1 Single 0.0 Type1

defaultEnemy :: Vector Float -> Actor
defaultEnemy p = newEnemy { enemyPosition = addVec [120,0,0] (mulScalarVec 60.0 p)
                          , enemyVelocity = mulScalarVec 30.0 p
                          , enemyOrientation = fromAxisAngleQ 0.0 0.0 1.0 (degToRad 180) 
                          , enemyOmega = [0.0, 0.0, -2.0] 
                          } 

rotatingEnemy = newEnemy { enemyOmega = [0.0, 0.0, 3.0] }

position :: Actor -> Vector Float
position actor@Player{} = playerPosition actor
position actor@Enemy{} = enemyPosition actor
position actor = [0,0,0]

velocity :: Actor -> Vector Float
velocity actor@Player{} = playerVelocity actor
velocity actor@Enemy{} = enemyVelocity actor
velocity actor = [0,0,0]

bullet :: Actor
bullet = Bullet "bullet" Opponent 1.0 zeroV zeroV zeroV passthru

explosion :: Vector Float -> Actor
explosion p = Explosion "explosion" p 1.5 8.0

passthru :: Actor -> Actors
passthru a = [a]

getPlayer :: Actors -> Actor
getPlayer (player:actors) = player

instance Show Actor where
  show p@Player{} = "player:" ++ show (playerShootingTimer p) ++ "\n"
  show b@Bullet{} = "bullet:" ++ show (bulletAge b) ++ "\n"
  show e@Enemy{} = "enemy: " ++ show (enemyAge e) ++ "\n"
  show sa@StaticActor{} = "static actor:" ++ (staticActorName sa) ++ "\n"
  show a = show "actor" ++ "\n"

isPlayer :: Actor -> Bool
isPlayer Player{} = True
isPlayer _ = False

isEnemy :: Actor -> Bool
isEnemy Enemy{} = True
isEnemy _ = False

isStatic :: Actor -> Bool
isStatic StaticActor{} = True
isStatic _ = False

isAlive :: Actor -> Bool
isAlive e@Enemy{} = enemyAge e > 0.0
isAlive b@Bullet{} = bulletAge b > 0.0
isAlive e@Explosion{} = explosionAge e > 0.0
isAlive _ = True
