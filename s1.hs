module S1 where

import Control.Monad.State.Strict

d :: (String, (Int,Float))
d = ( "aab", (4, 2.0))

foo :: (String, (Int, Float)) -> (String, (Int, Float))
foo (a, (i, f)) = ( a', (i, f))
    where a' = foldl (++) a (replicate i a)

-- state program that modify the passed state and produce value based on initial state
stateProgram :: State (Int,Int) String
stateProgram = do
  (s1, s2) <- get
  put (s1+2, s2-2)
  return $ (concat . replicate s1) "yyz"

-- state program with initial value run thru the State (Int,Int) and modified at the end 
stateProgram2 :: String -> State (Int,Int) String
stateProgram2 str = do
  (s1, s2) <- get
  return $ (foldl (++) (reverse str) (replicate s1 str))

run state = runState (stateProgram2 "aab") state

run2 state = (fst $ run state) ++ (fst $ run state)

-- stateProgram produce result based on state passed to it and then stateProgram2 transform the input based on the new state 
test = runState (stateProgram >>= stateProgram2) (1,2)