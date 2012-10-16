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

type LocalState = (ImmutableState, MutableState)

data BehaviourTreeNode = Condition (ImmutableState -> MutableState -> MutableState) LocalState
                       | Action (ImmutableState -> MutableState -> MutableState) LocalState

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

traverse' :: (a -> b) -> (b -> Bool) -> Tree a -> [b]
traverse' f c (Node v []) = [f v]
traverse' f c (Node v (n:ns)) = ns' ++ n'
  where ns' = traverse' f c (Node v ns)
        n' = filter c (traverse' f c n)

traverse'' :: (a -> a) -> (a -> Bool) -> Tree a -> Tree a
traverse'' f c (Node v []) = Node (f v) []
traverse'' f c (Node v ns) = Node (f v) (filter c' (map (traverse'' f c) ns))
  where c' (Node value ns) = c value


ntoa :: (Num a, Show a) => a -> String
ntoa i = show i

btnToA :: BehaviourTreeNode -> String
btnToA (Condition f s) = "Condition:" ++ show s
btnToA (Action f s) = "Action:" ++ show s

hermite :: Float -> Float
hermite x = a*x^5 + b*x^3 + c*x + d
  where a =  1.0
        b = -10.0
        c = 15.0
        d = 0

emptyLocalState = (IMS [], MS [])

theTree :: BehaviourTree
theTree = Node (Condition passThru emptyLocalState) [ Node (Condition passThru emptyLocalState) [ Node (Action addValue emptyLocalState) []
                                                                            , Node (Action addValue emptyLocalState) []
                                                                            ]
                                          , Node (Condition passThru emptyLocalState) [ Node (Action addValue emptyLocalState) []
                                                                         , Node (Action changeValue emptyLocalState) []
                                                                         ]
                                          , Node (Action changeValue emptyLocalState) []
                                          ]

theImmutableState = IMS [1.0, 2.0, 3.0]
ims = theImmutableState

processTreeActions :: BehaviourTree -> MutableState -> MutableState
processTreeActions tree ms = process actions ms
  where process :: [BehaviourTreeNode] -> MutableState -> MutableState
        process [] ms = ms
        process (a:as) ms = process as (action a ms)

        actions = flatten $ traverse'' transformer evaluator theTree

        action :: BehaviourTreeNode -> MutableState -> MutableState
        action (Condition rule s) ms = ms'
          where ms' = rule ims ms
        action (Action rule s) ms = ms'
          where ms' = rule ims ms

evaluator :: BehaviourTreeNode -> Bool
evaluator (Condition f s) = length s' == 0
  where (IMS s1, MS s2) = s
        s' = zip s1 s2
evaluator (Action f s) = True

transformer :: BehaviourTreeNode -> BehaviourTreeNode
transformer (Condition f (IMS s1, MS s2)) = (Action f (IMS s1, MS s2))
transformer (Action f (IMS s1, MS s2)) = (Condition f (IMS s1, MS s2))

mutableStateRef :: IORef MutableState
mutableStateRef = unsafePerformIO $ newIORef (MS [3.0,4.0,5.0])

simulate :: IO ()
simulate = do mutableState <- readIORef mutableStateRef
              writeIORef mutableStateRef (processTreeActions theTree mutableState)

empty :: a -> a
empty a = a

main :: IO ()
main = do replicateM_ 5 simulate >> readIORef mutableStateRef >>= print
          putStr $ drawTree $ fmap btnToA (traverse'' transformer evaluator theTree )

          print $ map hermite [0.0, 0.01 .. 1.0]


