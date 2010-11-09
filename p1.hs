import Data.IORef
import System.Random
import System.IO.Unsafe

randFloat :: IO Float
randFloat = randomIO

grid :: [Float]
grid = [1..10]


main = do let grid' = mapM (\_ -> do x <- randFloat
                                     return x) grid
          
          g <- grid'
          print g
          g <- grid'
          print g
