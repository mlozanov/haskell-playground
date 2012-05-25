module Actor where

data Actor = SimpleActor
           | Actor [Double]
             deriving (Eq, Ord, Show)

type Actors = [Actor]

