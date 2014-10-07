module Main where

import Backend
import Procedural

main :: IO ()
main = setup 1280 720 "sharpshooter \\ procedural" setupAction renderActions simulate ioActions

foo :: Int -> Int
foo a = a + a
