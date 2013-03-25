module Ai where

import Math

import Actor
import World

data Seed = EmptySeed 
          | Seed { seedTimer :: Float
                 , seedPosition :: Vector Float
                 , seedCallback :: World -> Actors -> Actors
                 }

seedEmpty w as = as

seedDefaultTime :: Float 
seedDefaultTime = 1.0

seed :: Seed
seed = Seed seedDefaultTime zeroV seedEmpty


data Behaviour = Seeker {}
               | Coward {}

