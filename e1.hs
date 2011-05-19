module Main 
    where

import Control.Monad
import Control.Monad.State 
import Control.Concurrent
import System.Random

data App = App (Float,Float) deriving Show

mainLoop :: App -> IO ()
mainLoop app@(App (a,b)) = if a > 0.0 
                           then loop
                           else return ()
    where 
      app'@(App (a',b')) = execState update app

      loop :: IO ()
      loop = do x <- randomRIO (-1.0, 1.0)
                display app'
                
                mainLoop (App (a',x))

      display :: App -> IO ()
      display app@(App (a,b)) = if (b > 0.0) 
                                then print app
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
main = mainLoop (App ((100000.0), 2.0))

