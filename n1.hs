module N1 where

data Perceptron a = P a deriving Show

instance Functor Perceptron where
    fmap f (P x) = P (f x)

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1.0 + exp (-x))

dsigmoid :: Double -> Double
dsigmoid x = sigmoid x * (1.0 - sigmoid x)

test :: Integer -> Integer
test x = x * 2

test1 :: [Integer] -> [Integer]
test1 xs = map (^2) xs

t1 = fmap test1 (P [1 .. 10])

perceptron :: [Double] -> Double
perceptron is = result
    where result = sigmoid (wsum - t)
          wsum = sum $ map (\(w,i) -> (w * i)) inputs
          inputs = zip weights is
          weights = [0.12, 0.0020, 0.02]
          t = 0.9

evalOutput :: Double -> Bool
evalOutput x | x > 0.31 = True
             | otherwise = False

perceptronTest = perceptron [0.0,0.0,1.0]

fn = evalOutput perceptronTest


superFormula phi m a b = result
    where
      ns = [2, 5, 7]
      f = m * phi * 0.25
      c = (cos f) / a
      s = (sin f) / b
      r = c ** (ns !! 1) + c ** (ns !! 2)
      result = r ** (-1 / (ns !! 0))