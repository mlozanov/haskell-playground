module Main where

import Data.Functor

class Tickable t where
    tick :: t -> t

data Player = Player Float Float
data Enemy = Enemy Float Float

instance Show Player where
    show (Player x y) = show x ++ ":"  ++ show y

instance Tickable Player where
    tick (Player x y) = Player x' y'
        where x' = x + 1
              y' = y + 1

initialPlayer :: Player
initialPlayer = Player 0 0

tickState :: Player -> Player
tickState p = tick p

main :: IO ()
main = do print $ tickState initialPlayer
          print $ tick (Enemy 1 2)