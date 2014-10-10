module Input where

import qualified Data.Set as S

import Graphics.UI.GLFW as GLFW

import Math

data Input = Input { inputAxisL :: Vector Float
                   , inputAxisR :: Vector Float
                   , inputButtons :: [Bool]
                   , inputKeys :: S.Set GLFW.Key
                   , inputMousePosition :: (Int,Int)
                   , inputMouseButtons :: (Bool,Bool)
                   , inputJoystickAxisL :: Vector Float
                   , inputJoystickAxisR :: Vector Float
                   , inputJoystickButtons :: [Bool]
                   } deriving Show

keySpace i = S.member (GLFW.CharKey ' ') (inputKeys i)

btnTriangle i = head $ inputJoystickButtons i
btnCircle   i = head $ drop 1 (inputJoystickButtons i)
btnCross    i = head $ drop 2 (inputJoystickButtons i)
btnSquare   i = head $ drop 3 (inputJoystickButtons i)

btnL1       i = head $ drop 4 (inputJoystickButtons i)
btnL2       i = head $ drop 5 (inputJoystickButtons i)
btnR1       i = head $ drop 6 (inputJoystickButtons i)
btnR2       i = head $ drop 7 (inputJoystickButtons i)

btnStart    i = head $ drop 8 (inputJoystickButtons i)
btnSelect   i = head $ drop 9 (inputJoystickButtons i)

--xbox360 controller

btnA i = head $ inputJoystickButtons i
btnB i = head $ drop 1 $ inputJoystickButtons i
