{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.IO.Unsafe
import System.Random

import Control.Monad

import Data.Tree
import Data.Traversable
import Data.IORef

data MutableState = MS [Float] deriving Show

data ImmutableState = IMS [Float] deriving Show

data BehaviourTreeNode = Condition (ImmutableState -> MutableState -> MutableState)
                       | Action (ImmutableState -> MutableState -> MutableState)

type BehaviourTree = Tree BehaviourTreeNode

passThru :: (ImmutableState -> MutableState -> MutableState)
passThru ims ms = ms

addValue :: (ImmutableState -> MutableState -> MutableState)
addValue ims (MS fs) = MS (fs ++ (10.0*(head fs)):[])

changeValue :: (ImmutableState -> MutableState -> MutableState)
changeValue ims (MS fs) = MS (map f fs)
  where f v = if (v > 2.0)
              then v-1.0
              else v

testTree = Node 1.0 [ Node 2.0 []
                    , Node 3.0 [ Node 4.0 []
                               , Node 5.0 []
                               , Node 6.0 []
                               ]
                    , Node 7.0 [ Node 8.0 []
                               , Node 9.0 []
                               ]
                    , Node 9.5 []
                    ]

test = traverse t testTree

t :: Double -> (Double -> Double) -> Double
t x f = f x + 1

theTree = Node (Condition passThru) [ Node (Condition passThru) [ Node (Action addValue) []
                                                                , Node (Action addValue) []
                                                                ]
                                    , Node (Condition passThru) [ Node (Action addValue) []
                                                                , Node (Action changeValue) []
                                                                ]
                                    , Node (Action changeValue) []
                                    ]

theImmutableState = IMS [1.0, 2.0, 3.0]
ims = theImmutableState

processTreeActions :: BehaviourTree -> MutableState -> MutableState
processTreeActions tree ms = process actions ms
  where process :: [BehaviourTreeNode] -> MutableState -> MutableState
        process [] ms = ms
        process (a:as) ms = process as (action a ms)

        actions = flatten tree

        action :: BehaviourTreeNode -> MutableState -> MutableState
        action (Condition rule) ms = ms'
          where ms' = rule ims ms
        action (Action rule) ms = ms'
          where ms' = rule ims ms

--processTreeActions :: BehaviourTree -> MutableState -> MutableState
--processTreeActions tree ms = process actions ms
--  where process :: [BehaviourTreeNode] -> MutableState -> MutableState
--        process [] ms = ms
--        process (a:as) ms = process as (action a ms)

--        actions = flatten tree

--        action :: BehaviourTreeNode -> MutableState -> MutableState

--        action (Condition rule) ms = ms'
--          where ms' = rule ims ms

--        action (Action rule) ms = ms'
--          where ms' = rule ims ms

mutableStateRef = unsafePerformIO $ newIORef (MS [3.0,4.0,5.0])

simulate = do mutableState <- readIORef mutableStateRef
              writeIORef mutableStateRef (processTreeActions theTree mutableState)

main = replicateM_ 5 simulate >> readIORef mutableStateRef >>= print


