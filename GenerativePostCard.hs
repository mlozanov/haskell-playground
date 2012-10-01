{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO.Unsafe
import System.Random

import Control.Monad
import Control.Applicative

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
main = do assemble genesis setup animate >>= print
