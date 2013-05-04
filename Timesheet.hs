{-# LANGUAGE FlexibleInstances #-}

module Timesheet where

import Control.Monad.State.Strict
import qualified Data.Map as M

import System.Random

import Math
import Actor
import World
import Enemies

type Timesheet a = M.Map a Actors
type Events a = (a, (a -> Actors))

instance Show ([Float] -> Actors) where
  show callback = "callback"

executeEvent :: (Eq a, Ord a) => Timesheet a -> a -> Actors
executeEvent timesheet timestamp = fromMaybe' $ M.lookup timestamp timesheet
  where fromMaybe' (Just v) = v
        fromMaybe' Nothing = []

newTimesheetRow :: (Eq a, Ord a) => Timesheet a -> (a, Actors) -> Timesheet a
newTimesheetRow timesheet (k, actors) = M.insert k actors timesheet 

lineOfEnemies origin = map enemy positionsAndVelocities
  where positionsAndVelocities = [ origin `addVec` [x,y,0.0] | x <- [4.0], y <- [-1.5, 0.0, 1.5] ]
        enemy = simpleEnemy

lineOfEnemiesHorizondal origin = map enemy positionsAndVelocities
  where positionsAndVelocities = map (addVec origin) [ [-1.5, 0.2, 0.0], [0.0, 0.4, 0.0], [1.5, 0.6, 0.0] ]
        enemy = rotatorEnemy

circleOfEnemies v = map defaultEnemy positions
  where positions = map circleVec [-pi, -(pi - (pi/v)) .. pi]

boss = map (\e -> e { enemyAge = 50.0, enemyVelocity = mulScalarVec 0.2 (enemyVelocity e), enemyShootingRate = 0.8, enemyTag = Boss1 }) (circleOfEnemies 12)

stageOneLinearWaves = concat [ [(x, lineOfEnemiesHorizondal), (x + 100, lineOfEnemies)] | x <- [10, 220 .. 2500] ]

stageOneTimesheet :: World -> Timesheet Int
stageOneTimesheet world = M.fromList $ (initialWave world) ++ [(2600, circleOfEnemies 3), (2820, boss)]
  where rnds :: [Float]
        rnds = take (length stageOneLinearWaves) (randoms (gen world))

        rndOffset :: World -> Float
        rndOffset w = randomFloatR w (-2.5) 2.5

        initialWave w = map (\(t, f) -> (t, f [10.0, rndOffset w, 0.0])) stageOneLinearWaves

