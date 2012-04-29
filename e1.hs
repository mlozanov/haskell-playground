module Main 
    where

import Control.Monad
import Control.Monad.State 
import Control.Concurrent
import System.Random

data App = App (Float,Float)

mainLoop :: App -> IO ()
mainLoop app@(App (a,b)) = if a > 0.0 
                           then loop app'
                           else return ()
    where 
      app' = execState update app

      loop :: App -> IO ()
      loop ap@(App (a,b)) = do x <- randomRIO (-1.0, 1.0 :: Float)
                               display ap
                               mainLoop (App (a,x))

      display :: App -> IO ()
      display app@(App (a,b)) = if (a > 0.0) 
                                then print (a,b)
                                else return ()

      update :: State App App
      update = do App (a,b) <- get
                  put (App (minus a,fun b))
                  app <- get
                  return app

      fun :: Float -> Float
      fun x = x*x*x + exp x + 1 / x

      minus :: Float -> Float
      minus a = a - 0.1

main :: IO ()
main = do replicateM_ 10 (forkIO $ mainLoop appState)
          --forever $ return ()
              where appState = App (4.0, 2.0)