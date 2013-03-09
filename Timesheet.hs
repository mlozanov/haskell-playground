module Timesheet where

import qualified Data.Map as M

import Math
import Actor

type Timesheet a = M.Map a Actors
type Events a = (a, (a -> Actors))

executeEvent :: (Eq a, Ord a) => Timesheet a -> a -> Actors
executeEvent timesheet timestamp = fromMaybe' $ M.lookup timestamp timesheet
  where fromMaybe' (Just v) = v
        fromMaybe' Nothing = []

newTimesheetRow :: (Eq a, Ord a) => Timesheet a -> (a, Actors) -> Timesheet a
newTimesheetRow timesheet (k, actors) = M.insert k actors timesheet 

lineOfEnemies = map defaultEnemy positions
  where positions = [ [x,y,0.0] | x <- [-1.0], y <- [-1.0, 0.0, 1.0] ]

circleOfEnemies v = map defaultEnemy positions
  where positions = map circleVec [-pi, -(pi - (pi/v)) .. pi]

boss = circleOfEnemies 10

stageOneTimesheet :: Timesheet Int
stageOneTimesheet = M.fromList []
                    --M.fromList [(0, circleOfEnemies 2 ++ lineOfEnemies)
                    --           ,(120, circleOfEnemies 4) 
                    --           ,(320, circleOfEnemies 6) 
                    --           ,(620, circleOfEnemies 3) 
                    --           ,(1020, boss)
                    --           ]

