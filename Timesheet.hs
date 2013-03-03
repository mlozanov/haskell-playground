module Timesheet where

import qualified Data.Map as M

import Math
import Actor

type Timesheet a = M.Map a Actors

newTimesheetRow :: (Eq a, Ord a) => Timesheet a -> (a, Actors) -> Timesheet a
newTimesheetRow timesheet (k, actors) = M.insert k actors timesheet 

stageOneTimesheet :: Timesheet Int
stageOneTimesheet = M.fromList [(0, map defaultEnemy (map circleVec [-pi, -(pi - (pi/2)) .. pi]))
                               ,(120, map defaultEnemy (map circleVec [-pi, -(pi - (pi/4)) .. pi])) 
                               ,(320, map defaultEnemy (map circleVec [-pi, -(pi - (pi/6)) .. pi])) 
                               ,(620, map defaultEnemy (map circleVec [-pi, -(pi - (pi/3)) .. pi])) 
                               ,(1020, map defaultEnemy (map circleVec [-pi, -(pi - (pi/10)) .. pi])) 
                               ]

