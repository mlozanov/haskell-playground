module Actor where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Objects as O

import qualified Data.Map as M

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

--type Group = [Actor]
--type Actors = [Group]

type Actors = [Actor]

data ActorKey = ActorKeyBackground | ActorKeyDynamic deriving Show

type ActorsMap = M.Map ActorKey Actors

instance Show Actor where
  show p@Player{} = "player:" ++ show (playerShootingTimer p) ++ "\n"
  show b@Bullet{} = "bullet:" ++ show (bulletAge b) ++ "\n"
  show e@Enemy{} = "enemy: " ++ show (enemyAge e) ++ "\n"
  show sa@StaticActor{} = "static actor:" ++ (staticActorName sa) ++ "\n"
  show a = show "actor" ++ "\n"

actorsmap :: ActorsMap
actorsmap = M.empty

newPlayer :: Actor
newPlayer = Player "player" zeroV identityQ zeroV zeroV 0.15 0.0

newEnemy :: Actor
newEnemy = Enemy "enemy" 120.0 zeroV identityQ zeroV zeroV zeroV 0.1 Single 0.0 Type1

defaultEnemy :: Vector Float -> Actor
defaultEnemy p = newEnemy { enemyPosition = addVec [120,0,0] (mulScalarVec 60.0 p)
                          , enemyVelocity = mulScalarVec 30.0 p
                          , enemyOrientation = fromAxisAngleQ 0.0 0.0 1.0 (degToRad 90)
                          , enemyOmega = [0.0, 0.0, -2.0]
                          }

rotatingEnemy = newEnemy { enemyOmega = [0.0, 0.0, 3.0] }

position :: Actor -> Vector Float
position actor@Player{} = playerPosition actor
position actor@Enemy{} = enemyPosition actor
position actor@Bullet{} = bulletPosition actor
position actor@StaticActor{} = staticActorPosition actor
position actor = zeroV

orientation :: Actor -> Quaternion Float
orientation actor@Player{} = playerOrientation actor
orientation actor@Enemy{} = enemyOrientation actor
orientation actor@StaticActor{} = staticActorOrientation actor
orientation actor = identityQ

velocity :: Actor -> Vector Float
velocity actor@Player{} = playerVelocity actor
velocity actor@Enemy{} = enemyVelocity actor
velocity actor = zeroV

acceleration :: Actor -> Vector Float
acceleration actor@Player{} = playerAcceleration actor
acceleration actor@Enemy{} = enemyAcceleration actor
acceleration actor = zeroV

setAcceleration :: Vector Float -> Actor -> Actor
setAcceleration newAcc player@Player{} = player { playerAcceleration = newAcc }
setAcceleration newAcc enemy@Enemy{} = enemy { enemyAcceleration = newAcc }
setAcceleration _ a = a

setVelocity :: Vector Float -> Actor -> Actor
setVelocity newVel player@Player{} = player { playerVelocity = newVel }
setVelocity newVel enemy@Enemy{} = enemy { enemyVelocity = newVel }
setVelocity _ a = a

setPosition :: Vector Float -> Actor -> Actor
setPosition newPosition p@Player{} = p { playerPosition = newPosition }
setPosition newPosition e@Enemy{} = e { enemyPosition = newPosition }
setPosition newPosition s@StaticActor{} = s { staticActorPosition = newPosition }
setPosition _ a = a

setOrientation :: Quaternion Float -> Actor -> Actor
setOrientation newQ s@StaticActor = s { staticActorOrientation = newQ }
setOrientation _ a = a

bullet :: Actor
bullet = Bullet "bullet" Opponent 1.0 zeroV zeroV zeroV passthru

explosion :: Vector Float -> Actor
explosion p = Explosion "explosion" p 1.5 8.0

passthru :: Actor -> Actors
passthru a = [a]

getPlayer :: Actors -> Actor
getPlayer (player:actors) = player

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

mkCircle :: Float -> Actor -> Shape Float
mkCircle r e = Circle (Actor.position e) r
