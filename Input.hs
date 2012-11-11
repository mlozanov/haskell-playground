module Input where

import Math

data Input = Input { inputAxisL :: Vector Float
                   , inputAxisR :: Vector Float
                   , inputButtons :: [Bool]
                   , inputMousePosition :: (Int,Int)
                   , inputMouseButtons :: (Bool,Bool)
                   } deriving Show

