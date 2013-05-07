module Behaviours where

import Math
import Actor

followTarget :: Float -> Actor -> Actor -> Actor
followTarget dt target actor@Enemy{} = actor { enemyVelocity = v }
  where v = clampV 60.0 ((velocity actor) `addVec` direction)
        direction = (position target) `subVec` (position actor)

followTarget dt target actor = actor

fleeTarget :: Float -> Actor -> Actor -> Actor
fleeTarget dt target actor@Enemy{} = actor { enemyVelocity = v }
  where v = if distance < 50.0 
            then clampV 60.0 ((velocity actor) `addVec` direction)
            else velocity actor
        direction = (position actor) `subVec` (position target)
        distance = lengthVec direction

fleeTarget dt target actor = actor

