module Enemies where

import Math
import Actor

simpleEnemy :: Vector Float -> Actor
simpleEnemy p = newEnemy { enemyPosition = addVec [120,0,0] (mulScalarVec 60.0 p)
                         , enemyVelocity = mulScalarVec 20.0 p
                         , enemyOrientation = fromAxisAngleQ 0.0 0.0 1.0 (degToRad 180) 
                         , enemyOmega = zeroV
                         , enemyShootingRate = 2.0
                         } 

rotatorEnemy :: Vector Float -> Actor
rotatorEnemy p = newEnemy { enemyPosition = addVec [120,0,0] (mulScalarVec 60.0 p)
                          , enemyVelocity = mulScalarVec 30.0 p
                          , enemyOrientation = fromAxisAngleQ 0.0 0.0 1.0 (degToRad 180) 
                          , enemyOmega = [0.0, 0.0, -10.0] 
                          , enemyShootingRate = 1.6
                          } 

curveEnemy :: Float -> Vector Float -> Actor
curveEnemy radius p = undefined

splineEnemy :: Spline Float -> Actor
splineEnemy spline = undefined