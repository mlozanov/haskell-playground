import Data.IORef
import System.Random
import System.IO.Unsafe
import qualified Data.Graph as G

randFloat :: IO Float
randFloat = randomIO

grid :: [Float]
grid = [1..10]

v :: (Int, Int) -> (G.Vertex, G.Vertex)
v (a,b) = (a, b)

es :: [G.Edge]
es = [v (0,1), v (0,2), v (1,3), v (1,4), v (1,5), v (2,6), v (2,7), v (3,8), v (3,9)]

gra = G.buildG (0, 9::G.Vertex) es


main = do let grid' = mapM (\_ -> do x <- randFloat
                                     return x) grid
          
          g <- grid'
          print g
          g <- grid'
          print g
