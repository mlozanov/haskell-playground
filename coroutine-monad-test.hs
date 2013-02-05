module Main where

import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

ai :: Coroutine (Yield Int) IO String
ai = do yield 1
        return "1"
        yield 2
        return "2"
        yield 3
        return "3"

ai2 :: Coroutine (Yield Int) IO String
ai2 = do yield 10
         return "10"

runAi :: Show a => Coroutine (Yield a) IO b -> IO b
runAi c = pogoStick (\(Yield x cont) -> lift (print x) >> cont) c

stepAi :: Show a => Coroutine (Yield a) IO b -> Coroutine (Yield a) IO b
stepAi c = bounce (\(Yield x cont) -> lift (print x) >> cont) c

runAiWithState :: Int -> Coroutine (Yield Int) IO b -> Coroutine (Yield Int) IO b -> IO (Int, b)
runAiWithState s c c2 = foldRun f s c
    where f st (Yield x cont) = if x == 4 then (st, c2)
                                else (st, cont)

--ai2 :: Coroutine (Await Int) IO String
--ai2 = do await 3 "zz"
--         return "s"
--         yield 3
--         return "z"


main :: IO ()
main = runAi ai >>= print >>
       --runAi (stepAi ai) >>= print
       runAiWithState 10 ai ai2 >>= print
