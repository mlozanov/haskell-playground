{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO.Unsafe
import System.Random

import Control.Monad
import Control.Applicative

import Data.Tree
import Data.Traversable

theTree :: Tree Float
theTree = Node 0 [ Node 1 [ Node 3 []
                          , Node 4 []
                          ]
                 , Node 2 [ Node 5 []
                          , Node 6 [Node 7 []]
                          ] 
                 ]


traverse' :: (Fractional a, Ord a) => (a -> b) -> (a -> Bool) -> Tree a -> [b]
traverse' f fb (Node v []) = [f v]
traverse' f fb (Node v (n:ns)) = ns' ++ n'
  where ns' = traverse' f fb (Node v ns)
        n'  = if (fb v) then traverse' f fb n
              else []

traverse'' :: (Fractional a, Ord a) => (a -> a) -> Tree a -> Tree a
traverse'' f (Node v []) = Node (f v) []
traverse'' f (Node v ns) = Node (f v) (filter (fbn) (map (traverse'' f) ns))


fb v | v == 0 = True
     | v == 2 = True
     | v > 5 = True
     | otherwise = False

fbn (Node v _) = fb v


f :: Float -> Float
f x = x

data World = World StdGen [Float] deriving Show

setup :: World -> IO World
setup world = return world

animate :: World -> IO World
animate world@(World g (x:xs)) = return (World g xs)

assemble :: World -> (World -> IO World) -> (World -> IO World) -> IO World
assemble world setupCallback animateCallback = (setupCallback world) >>= animate' 
  where 
    animate' :: World -> IO World
    animate' w@(World g []) = return w
    animate' w = print w >> animateCallback w >>= animate'

genesis :: World 
genesis = World (mkStdGen 1023) [1,2,3,4,5]

main :: IO ()
main = do print $ traverse' f fb theTree
          print $ theTree

          assemble genesis setup animate >>= print

          putStr $ drawTree $ fmap (\i -> show i) (traverse'' f theTree)
