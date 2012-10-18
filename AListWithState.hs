{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC #-}

module Main where

data A = A1 Int
       | A2 Int Int
       | A3 Int Int Int
       | AF Int (Int -> Int) -- first parameter of constructor represent the start for that data. second is some tranforming function
       deriving Show

instance Show (Int -> Int) where
  show f = "func"

type As = [A]

data GlobalState = GS { gs0 :: Int, gs1 :: Float }

gs :: GlobalState
gs = GS 100 3.14159

actions :: As
actions = [ A1 2, A2 2 3, A1 3, AF 10 (+ 1) ]

-- this one evaluate data and crunch it to single value
evaluator :: GlobalState -> A -> Int
evaluator gs (A1 a) = a - gs0 gs
evaluator gs (A2 a b) = b + gs0 gs - a
evaluator gs (A3 a b c) = c + b - a - gs0 gs
evaluator gs (AF a f) = f (gs0 gs) + a

-- this one transform the data to a new one
-- this is a way to transform local state for each piece of data - AF has local state and function
-- probable there is monad for this one
transformer :: GlobalState -> A -> A
transformer gs (A1 a) = A2 a a
transformer gs (A2 a b) = A1 (a+b+gs0 gs)
transformer gs (A3 a b c) = A2 c b
transformer gs (AF a f) = AF (f a) (+ (gs0 gs))

-- TODO
-- find a way how different elements from the list will communicate between themselves - exchanging the local state they have. do I need this at all?

main = do print actions
          print $ map (evaluator gs) actions
          print $ map (evaluator gs) $ map (transformer gs) actions


