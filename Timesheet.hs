module Timesheet where

import Actor

data TimesheetRow a = TimesheetRow (a, Actors)
type Timesheet a = [TimesheetRow a]

