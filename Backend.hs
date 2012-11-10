module Backend (
  setup, 
  finalize, 
  emptyWorld,
  joystickCallback,
  World(..), 
  Input(..) ) 
  where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT.Objects as O
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import System.Random
import System.Mem
import System.CPUTime
import Foreign.Ptr
import Foreign.Marshal.Array

import Graphics
import Math
import Camera
import Actor

import Vbo
import Fbo
import Shader

data Input = Input { inputAxisL :: Vector Float
                   , inputAxisR :: Vector Float
                   , inputButtons :: [Bool]
                   , inputMousePosition :: (Int,Int)
                   , inputMouseButtons :: (Bool,Bool)
                   } deriving Show


data World = World { worldTime :: Float
                   , worldInput :: Input 
                   , cameras :: Cameras
                   , actors :: Actors 
                   } deriving Show

emptyWorld :: World
emptyWorld = (World 0.0 i cs as)
    where i = Input zeroV zeroV [False, False, False, False, False, False, False, False] (0,0) (False,False)
          cs = [EmptyCamera]
          as = []

setup :: Int -> Int -> IORef World -> IO ()
setup wx wy worldRef = do 
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size (fromIntegral wx) (fromIntegral wy)) [GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 24] GLFW.Window
  GLFW.windowTitle $= "sharpshooter"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  GL.pointSize  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0.18 0.18 0.18 1.0

  GL.depthFunc $= Just Lequal
   
  GL.colorMaterial $= Just (GL.FrontAndBack, GL.AmbientAndDiffuse)

  -- set 2D perspective view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
       do GL.viewport   $= (GL.Position 0 0, size)
          GL.matrixMode $= GL.Projection
          GL.loadIdentity

          let m' = Math.toList Math.perspective
          p <- newMatrix GL.RowMajor m' :: IO (GLmatrix GLfloat)
          multMatrix p

  --GLFW.keyCallback $= keyboardCallback
  GLFW.mousePosCallback $= mousePositionCallback worldRef
  GLFW.mouseButtonCallback $= mouseBtnCallback worldRef

finalize :: IO ()
finalize = do 
  GLFW.closeWindow
  GLFW.terminate

keyboardCallback :: IORef World -> KeyCallback
keyboardCallback worldRef key state = print key >> print state >> return ()

mousePositionCallback :: IORef World -> MousePosCallback
mousePositionCallback worldRef (Position x y) = modifyIORef worldRef readMousePosition
  where readMousePosition world = world { worldInput = input { inputMousePosition = (fromIntegral x, fromIntegral y) } }
          where input = worldInput world 

mouseBtnCallback :: IORef World -> MouseButtonCallback
mouseBtnCallback worldRef button state = modifyIORef worldRef readMouseButtons
  where readMouseButtons world = world { worldInput = input { inputMouseButtons = (b1,b2) } }
          where input = worldInput world
                (b1o,b2o) = worldInputMouseButtons world
                b1 | button == ButtonLeft = state == Press
                   | otherwise = b1o
                b2 | button == ButtonRight = state == Press
                   | otherwise = b2o

joystickCallback :: IORef World -> IO ()
joystickCallback worldRef = do 
  axises <- GL.get $ GLFW.joystickPos (GLFW.Joystick 0) 4
  bs <- GL.get $ GLFW.joystickButtons (GLFW.Joystick 0)

  modifyIORef worldRef (readJoystick axises bs)

  where readJoystick ([jlx,jly,jrx,jry]) buttonStates world = world { worldInput = input { inputAxisL = axisL, inputAxisR = axisR, inputButtons = bs } }
          where input = worldInput world
                axisL = [jlx,jly,0,0]
                axisR = [jrx,jry,0,0]
                bs = map (\s -> s == Press) buttonStates

worldInputButtons :: World -> [Bool]
worldInputButtons world = inputButtons (worldInput world)

worldInputMouseButtons :: World -> (Bool,Bool)
worldInputMouseButtons world = inputMouseButtons (worldInput world)
