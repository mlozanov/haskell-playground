module Timesheet where

import qualified Data.Map as M

import System.Random

import Math
import Actor
import World

type Timesheet a = M.Map a Actors
type Events a = (a, (a -> Actors))

executeEvent :: (Eq a, Ord a) => Timesheet a -> a -> Actors
executeEvent timesheet timestamp = fromMaybe' $ M.lookup timestamp timesheet
  where fromMaybe' (Just v) = v
        fromMaybe' Nothing = []

newTimesheetRow :: (Eq a, Ord a) => Timesheet a -> (a, Actors) -> Timesheet a
newTimesheetRow timesheet (k, actors) = M.insert k actors timesheet 

lineOfEnemies origin = map enemy positionsAndVelocities
  where positionsAndVelocities = [ [x,y,0.0] | x <- [8.0], y <- [-1.0, 0.0, 1.0] ]
        enemy p = newEnemy { enemyPosition = (mulScalarVec 60.0 (addVec origin p))
                           , enemyVelocity = [-60,0.0,0.0]
                           , enemyOrientation = fromAxisAngleQ 0.0 0.0 1.0 (degToRad 180) 
                           }

circleOfEnemies v = map defaultEnemy positions
  where positions = map circleVec [-pi, -(pi - (pi/v)) .. pi]

boss = circleOfEnemies 10

stageOneLinearWaves = [ (x, lineOfEnemies) | x <- [10, 220 .. 2500] ]

stageOneTimesheet :: World -> Timesheet Int
stageOneTimesheet world = M.fromList $ initialWave ++ [(2600, circleOfEnemies 3), (2820, boss)]
  where rnds :: [Float]
        rnds = take (length stageOneLinearWaves) (randoms (gen world))

        rndOffset :: Float
        rndOffset = fst $ random (gen world)

        initialWave = map (\(t, f) -> (t, f [0.0, 3.0 * (0.5 - rndOffset), 0.0])) stageOneLinearWaves

