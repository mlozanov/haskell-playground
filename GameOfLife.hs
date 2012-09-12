module Main where

import System.Random
import Control.Monad

type CellKind = Int
data Cell = Cell Int Int CellKind deriving Show
type Cells = [Cell]

--instance Show Cell where
--    show c@(Cell x y v) = show x ++ ":" ++ show y ++ ":" ++ show v ++ "\n"

data Command = Nop
             | Update Double
             | Quit

data World = World Command Cells

worldCmd w@(World cmd _) = cmd
worldCells w@(World _ cells) = cells

cellX c@(Cell x y value) = x
cellY c@(Cell x y value) = y
cellValue c@(Cell x y value) = value

takeAlive (a,_,_) = a
takeDead (_,b,_) = b
takeZombies (_,_,z) = z

--
isAlive :: Cell -> Bool
isAlive c = cellValue c == 1

isDead :: Cell -> Bool
isDead c = cellValue c == 0

isZombie :: Cell -> Bool
isZombie c = cellValue c == 2

findNeighbours :: Cells -> Cell -> Cells
findNeighbours cells c@(Cell x y _) = cells' x y
  where offsets = [ (-1, 0), (1, 0), (0, 1), (0, -1), (-1, 1), (-1, -1), (1, 1), (1, -1) ]

        cells' x' y' = [ c | (x,y) <- offsets, c <- cells, x + x' == cellX c && y + y' == cellY c ]

grid :: Cells
grid = [ Cell x y 0 | x <- [-16 .. 16], y <- [-16 .. 16] ]

rule :: Int -> Int -> Cell -> Cell
rule n z c@(Cell x y v) | z == 0 && n == 2 && v == 1 = Cell x y 1
                        | z == 0 && n == 3 = Cell x y 1
                        | z > 0 && v == 1 = Cell x y 2 -- new zombie is born
                        | otherwise = Cell x y 0 -- dead is everywere

step :: Double -> (Int -> Int -> Cell -> Cell) -> Cells -> Cells
step dt zerule cells = cells'
  where cells' = map (f cells) cells

        f :: Cells -> Cell -> Cell
        f cs c = zerule nbAliveCells nbZombieCells c
          where
            neighbours = findNeighbours cs c
            nbAliveCells = takeAlive $ countCells neighbours
            nbZombieCells = takeZombies $ countCells neighbours


countCells :: Cells -> (Int,Int,Int)
countCells cells = (alive, dead, zombies)
  where
    alive = length $ filter isAlive cells
    dead = length $ filter isDead cells
    zombies = length $ filter isZombie cells
--

cellKind :: Double -> CellKind
cellKind v | v < 0.01 = 2
           | v < 0.772 = 0
           | otherwise = 1

cellPicture :: CellKind -> String
cellPicture k | k == 0 = "."
              | k == 1 = "o"
              | k == 2 = "x"

render :: Cells -> IO ()
render [] = print "--------------------------------" >> return ()
render cs = printOneLine cs8 >> putStrLn "" >> render css
  where (cs8, css) = splitAt 33 cs

        printOneLine cs' = mapM_ p cs' 

        p c = putStr $ "|" ++ ((cellPicture . cellValue) c) 

main :: IO ()
main = do putStrLn "Game Of Life"

          vs <- replicateM (length grid) (randomRIO (0.0, 1.0 :: Double))

          print $ length vs

          loop $ World (Update 0.01) (zipWith (\c@(Cell x y _) v -> Cell x y (cellKind v)) grid vs)

  where loop :: World -> IO ()
        loop world = 
            case cmd of
              Update dt -> if dt < 10.0 then simulation dt world
                           else loop $ World Quit cells'
              Quit -> print "quit" >> return ()

          where cmd = worldCmd world
                cells = worldCells world
                cells' = step 0.01 rule cells

                simulation dt world = render cells >>
                                      loop (World (Update (dt+0.01)) cells')



