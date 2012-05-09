module Main where

import Control.Concurrent
import Control.Monad.State.Strict

type TheState = (Int,Int)

-- state program that modify the passed state and produce value based on initial state
stateProgram1 :: State TheState String
stateProgram1 = do (s1,s2) <- get
                   modify transform
                   return $ (concat . replicate s1) "yyz"
    where transform (s1,s2) = (s1+2, s2-2)

-- state program with initial value run thru the State TheState and modified at the end 
stateProgram2 :: String -> State TheState String
stateProgram2 str = do (s1, s2) <- get
                       return $ (foldl (++) (reverse str) (replicate s1 str))

-- not taking state into account
-- return is identity operator which just passes state forward for the next computation
stateProgram3 :: String -> State TheState String
stateProgram3 str = return (str ++ str ++ str)

stateProgram4 :: String -> State TheState String
stateProgram4 str = modify transformState >> return str

transformState :: TheState -> TheState
transformState (a,b) = (b, a)

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
test5 :: TheState -> IO ()
test5 (p,q) = do let (v, s) = updater (p,q)

                 print v

                 if (fst s) < 1000
                 then test5 s
                 else return ()

    where state = stateProgram1 >>= stateProgram4 >>= stateProgram3 >>= stateProgram2 >>= stateProgram4

          updater (a,b) = runState state (a,b)


main :: IO ()
main = do test5 (0,0)
