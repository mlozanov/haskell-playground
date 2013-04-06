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

btnTriangle i = b1
  where (b1:rest) = inputJoystickButtons i
btnCircle   i = b2
  where (b1:b2:rest) = inputJoystickButtons i
btnCross    i = b3
  where (b1:b2:b3:rest) = inputJoystickButtons i
btnSquare   i = b4
  where (b1:b2:b3:b4:rest) = inputJoystickButtons i

btnL1       i = b5
  where (b1:b2:b3:b4:b5:rest) = inputJoystickButtons i
btnL2       i = b6
  where (b1:b2:b3:b4:b5:b6:rest) = inputJoystickButtons i
btnR1       i = b7
  where (b1:b2:b3:b4:b5:b6:b7:rest) = inputJoystickButtons i
btnR2       i = b8
  where (b1:b2:b3:b4:b5:b6:b7:b8:rest) = inputJoystickButtons i

btnStart    i = b9
  where (b1:b2:b3:b4:b5:b6:b7:b8:b9:rest) = inputJoystickButtons i
btnSelect   i = b10
  where (b1:b2:b3:b4:b5:b6:b7:b8:b9:b10:rest) = inputJoystickButtons i

