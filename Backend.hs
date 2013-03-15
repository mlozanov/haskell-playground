module Backend (
  setup, 
  World(..), 
  RenderState(..),
  SetupAction,
  RenderAction,
  IOActions,
  SimulateAction
  )
  where


import System.Random
import System.Mem
import System.CPUTime

import Foreign.Ptr
import Foreign.Marshal.Array

import Data.IORef
import qualified Data.Map as M

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT.Objects as O
import Graphics.Rendering.OpenGL (($=))

import Control.Monad
import Control.Monad.State
import Control.Concurrent

import Input
import Graphics
import Math
import Camera
import Actor
import World

import Vbo
import Fbo
import Shader
import Primitives

data RenderState = RenderState { projectionMatrix :: Ptr GLfloat
                               , viewMatrix :: Ptr GLfloat
                               , modelMatrix :: Ptr GLfloat
                               , shaderProgramsMap :: M.Map String ShaderProgramData
                               , bufferObjectsMap :: M.Map String Vbo
                               }


type SetupAction = (IORef World -> IORef Actors -> IORef RenderState -> IO ())
type RenderAction = (IORef World -> IORef Actors -> IORef RenderState -> IO ())

type SimulateAction = Actors -> World -> (Actors, World) --(Float -> World -> World)
type IOActions = [(IORef World -> IO ())]

emptyWorld :: World
emptyWorld = (World 0 i 0.0166667 cs bs (mkStdGen 1023))
    where i = Input zeroV zeroV [False, False, False, False, False, False, False, False] (0,0) (False,False) zeroV zeroV [False, False]
          cs = [EmptyCamera]
          bs = [] :: Actors

setupOpenGL32 :: IO ()
setupOpenGL32 = sequence_ [GLFW.openWindowHint GLFW.OpenGLVersionMajor 3, GLFW.openWindowHint GLFW.OpenGLVersionMinor 2, GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile]

setup :: Int -> Int -> String -> SetupAction -> [RenderAction] -> SimulateAction -> IOActions -> IO ()
setup wx wy title setupAction renderActions simulateAction ioActions = do 
  GLFW.initialize
  -- open window

  --setupOpenGL32

  GLFW.openWindow (GL.Size (fromIntegral wx) (fromIntegral wy)) [GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 24] GLFW.Window
  GLFW.windowTitle $= title
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.2
  GL.pointSize  $= 1.0
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

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  projMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  viewMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  modelMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)

  let objects = M.fromList [] --createGeometryObjects

  let shaders = M.fromList [] --createShaderPrograms

  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray modelMatrixArray shaders objects)

  worldRef <- newIORef emptyWorld

  actorsRef <- newIORef ([] :: Actors)

  setupAction worldRef actorsRef renderStateRef

  GLFW.keyCallback $= keyboardCallback worldRef
  GLFW.mousePosCallback $= mousePositionCallback worldRef
  GLFW.mouseButtonCallback $= mouseBtnCallback worldRef

  -- invoke the active drawing loop
  mainLoop worldRef actorsRef renderStateRef renderActions simulateAction ioActions

  GLFW.closeWindow
  GLFW.terminate


mainLoop :: IORef World -> IORef Actors -> IORef RenderState -> [RenderAction] -> SimulateAction -> IOActions -> IO ()
mainLoop world actors renderState renderActions simulateAction ioActions = loop 0 world actors renderState
  where 
 
    loop :: Int -> IORef World -> IORef Actors -> IORef RenderState -> IO ()
    loop t worldRef actorsRef renderStateRef = do
      t0 <- getCPUTime

      updateJoystickState worldRef

      mapM_ (\action -> action worldRef) ioActions

      world <- readIORef worldRef
      actors <- readIORef actorsRef
      let (actors', world') = simulateAction actors world
      writeIORef worldRef (world' { worldTime = t })
      writeIORef actorsRef actors'

      mapM_ (\action -> action worldRef actorsRef renderStateRef) renderActions

      GLFW.swapBuffers

      --performGC

      t1 <- getCPUTime

      -- let dt = 0.0166667 
      let dt = ((fromIntegral (t1 - t0)) / (10^12)) :: Float

      -- writeIORef worldRef (world' { worldDt = dt })
      --let storeDt dt' w = w { worldDt = dt' } 
      -- in modifyIORef worldRef (storeDt dt)

      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
          -- sleep for 1ms to yield CPU to other applications
          GLFW.sleep 0.001

          -- only continue when the window is not closed
          windowOpenStatus <- getParam Opened
          unless (not windowOpenStatus) $
            loop (t + 1) worldRef actorsRef renderStateRef -- loop with next action


keyboardCallback :: IORef World -> KeyCallback
keyboardCallback worldRef key Press = modifyIORef worldRef readKeys  -- >> print key >> print state
  where readKeys world = world { worldInput = input { inputAxisL = axisL } }
          where input = worldInput world
                (x:y:zs) = inputAxisL input
                axisL = case key of
                          GLFW.CharKey 'D' -> (1.0:y:zs)
                          GLFW.CharKey 'A' -> ((-1.0):y:zs)
                          GLFW.CharKey 'W' -> (x:1.0:zs)
                          GLFW.CharKey 'S' -> (x:(-1.0):zs)
                          otherwise -> [x,y,0.0,0.0]

keyboardCallback worldRef key Release = modifyIORef worldRef readKeys  -- >> print key >> print state
  where readKeys world = world { worldInput = input { inputAxisL = axisL } }
          where input = worldInput world
                (x:y:zs) = inputAxisL input
                axisL = case key of
                          GLFW.CharKey 'D' -> (0.0:y:zs)
                          GLFW.CharKey 'A' -> (0.0:y:zs)
                          GLFW.CharKey 'W' -> (x:0.0:zs)
                          GLFW.CharKey 'S' -> (x:0.0:zs)
                          otherwise -> [x,y,0.0,0.0]

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

updateJoystickState :: IORef World -> IO ()
updateJoystickState worldRef = do 
  axises <- GL.get $ GLFW.joystickPos firstJoystick 4
  bs <- GL.get $ GLFW.joystickButtons firstJoystick

  modifyIORef worldRef (storeJoystickData axises bs)

  where firstJoystick = GLFW.Joystick 0
        storeJoystickData ([jlx,jly,jrx,jry]) buttonStates world = world { worldInput = input { inputJoystickAxisL = axisL, inputJoystickAxisR = axisR, inputJoystickButtons = bs } }
          where input = worldInput world
                axisL = [jlx,jly,0,0]
                axisR = [jrx,jry,0,0]
                bs = map (\s -> s == Press) buttonStates



