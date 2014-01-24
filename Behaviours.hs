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

circle :: Float -> Float -> Actor -> Actor
circle radius t = setPosition p
  where p = mulScalarVec radius (circleVec t)

trajectory :: Int -> Float -> Actor -> Actor
trajectory time dt player@Player{} = player
trajectory time dt e = setVelocity v e
  where v = [vx,vy,0.0]
        fTime = fromIntegral time / 60.0
        vx = 50.0 * sin (1.0 * fTime)
        vy = 50.0 * cos (1.0 * fTime)

keepDistance :: Vector Float -> Actor -> Actor
keepDistance target actor = undefined

steerOnSpline :: Spline Float -> Actor -> Actor
steerOnSpline spline actor = undefined

wander :: World -> Actor -> Actor
wander world actor@Player{} = actor
wander world actor = setVelocity v actor
  where r = mulScalarVec 45.0 $ fst $ rndPolarV g'
        sight = upV $ orientation actor
        p = position actor
        leadingPoint = addVec p (mulScalarVec 10.0 sight)
        v = mulScalarVec 20.0 (normalizeV $ subVec (addVec leadingPoint r) p)
        (g, g') = split (gen world)
        dt = worldDt world

explode :: Actor -> Actor
explode e@Enemy{} | enemyAge e <= 0.01 = explosion (enemyPosition e)
                  | otherwise = e
explode a = a

pathFollow :: World -> Actor -> Actor
pathFollow world actor = actor
