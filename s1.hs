module S1 where

import Control.Monad.State.Strict

d :: (String, (Int,Float))
d = ( "aab", (4, 2.0))

foo :: (String, (Int, Float)) -> (String, (Int, Float))
foo (a, (i, f)) = ( a', (i, f))
    where a' = foldl (++) a (replicate i a)

type TheState = (Int,Int)

-- state program that modify the passed state and produce value based on initial state
stateProgram :: State TheState String
stateProgram = do
  (s1, s2) <- get
  put (s1+2, s2-2)
  return $ (concat . replicate s1) "yyz"

-- state program with initial value run thru the State TheState and modified at the end 
stateProgram2 :: String -> State TheState String
stateProgram2 str = do
  (s1, s2) <- get
  return $ (foldl (++) (reverse str) (replicate s1 str))

-- not taking state into account
-- return is identity operator which just passes state forward for the next computation
stateProgram3 :: String -> State TheState String
stateProgram3 str = return (str ++ str ++ str)

stateProgram4 :: String -> State TheState String
stateProgram4 str = do
  (s1, s2) <- get
  put (s2,s1)
  return str

transformState :: TheState -> TheState
transformState (a,b) = (a+1, b+2)

run state = runState (stateProgram2 "aab") state

run2 state = (fst $ run state) ++ (fst $ run state)

-- stateProgram produce result based on state passed to it and then stateProgram2 transform the input based on the new state 
test = evalState state initialState
    where initialState = (5,2)
          state = stateProgram2 "zza" >>= stateProgram4 >>= stateProgram2 >>= stateProgram3 >>= stateProgram3

test2 = withState transformState theStateProgram
    where inputValue = "zzab"
          theStateProgram = stateProgram3 inputValue

test3 str (a,b) | length str == 0 = ""
                | otherwise = evalState state (a,b)
                where state = stateProgram2 str >>= stateProgram2

test4 str = test4' str (1,1)
    where test4' str (a,b) | length str < 40 = evalState state (a,b)
                           | otherwise = str
                           where state = stateProgram2 str >>= stateProgram2

-- testing how to loop states and to simulate a gameloop with mutable state passed implicitly
test5 :: IO ()
test5 = do let (v, s) = updater (5,1)

           print (v,s)

           if (fst s) < 10 
           then test5
           else return ()

    where state = stateProgram >>= stateProgram2 >>= stateProgram2

          updater (a,b) = runState state (a,b)

