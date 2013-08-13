module Behaviours where

import System.Random (split)

import Math
import Actor
import World

followTarget :: World -> Actor -> Actor -> Actor
followTarget world target actor@Enemy{} = actor { enemyVelocity = v }
  where v = clampV 60.0 ((velocity actor) `addVec` direction)
        direction = (position target) `subVec` (position actor)
        dt = worldDt world

followTarget world target actor = actor

fleeTarget :: World -> Actor -> Actor -> Actor
fleeTarget world target actor@Enemy{} = actor { enemyVelocity = v }
  where v = if distance < 50.0 
            then clampV 60.0 ((velocity actor) `addVec` direction)
            else velocity actor
        direction = (position actor) `subVec` (position target)
        distance = lengthVec direction
        dt = worldDt world

fleeTarget world target actor = actor

circleAroundPoint :: Vector Float -> Actor -> Actor
circleAroundPoint target actor = undefined

keepDistance :: Vector Float -> Actor -> Actor
keepDistance target actor = undefined

steerOnSpline :: Spline Float -> Actor -> Actor
steerOnSpline spline actor = undefined

wander :: World -> Actor -> Actor
wander world actor = setVelocity v actor
  where r = mulScalarVec 45.0 $ fst $ rndPolarV g'
        sight = upV $ orientation actor
        p = position actor
        leadingPoint = addVec p (mulScalarVec 10.0 sight)
        v = mulScalarVec 20.0 (normalizeV $ subVec (addVec leadingPoint r) p)
        (g, g') = split (gen world)
        dt = worldDt world
