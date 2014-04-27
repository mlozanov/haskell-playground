module Main where

import Backend
import Game

main :: IO ()
main = setup 1280 720 "sharpshooter" setupAction renderActions simulate ioActions
