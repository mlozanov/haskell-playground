module Backend (
  setup, 
  World(..), 
  RenderState(..),
  SetupAction,
  RenderAction,
  IOActions,
  InputActionPure,
  SimulateAction
  )
  where


import System.Random
import System.Mem
import System.CPUTime

import Foreign.Ptr
import Foreign.Marshal.Array

import Data.IORef
import Data.Map as M hiding (map)

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

import Vbo
import Fbo
import Shader
import Primitives

data World = World { worldTime :: !Float
                   , worldInput :: !Input 
                   , cameras :: !Cameras
                   , actors :: !Actors 
                   , gen :: !StdGen
                   } deriving Show


data RenderState = RenderState { projectionMatrix :: Ptr GLfloat
                               , viewMatrix :: Ptr GLfloat
                               , shaderProgramsMap :: Map String ShaderProgram
                               , bufferObjectsMap :: Map String Vbo
                               }


type SetupAction = (IORef World -> IO ())
type RenderAction = (Float -> IORef World -> IORef RenderState -> IO ())

type SimulateAction = (Float -> World -> World)
type IOActions = [(Float -> IORef World -> IO ())]
type InputActionPure = (Float -> World -> World)

emptyWorld :: World
emptyWorld = (World 0.0 i cs as (mkStdGen 1023))
    where i = Input zeroV zeroV [False, False, False, False, False, False, False, False] (0,0) (False,False)
          cs = [EmptyCamera]
          as = []

setup :: Int -> Int -> String -> SetupAction -> [RenderAction] -> InputActionPure -> SimulateAction -> IOActions -> IO ()
setup wx wy title setupAction renderActions inputAction simulateAction ioActions = do 
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size (fromIntegral wx) (fromIntegral wy)) [GLFW.DisplayAlphaBits 8, GLFW.DisplayDepthBits 24] GLFW.Window
  GLFW.windowTitle $= title
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.0
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

  worldRef <- newIORef emptyWorld

  setupAction worldRef

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  projMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  viewMatrixArray <- newArray $ replicate 16 (0.0 :: GLfloat)
  defaultProgram <- newProgram "../data/shaders/default.vert" "../data/shaders/default.frag" 
  sphericalProgram <- newProgram "../data/shaders/sph.vert" "../data/shaders/sph.frag"  

  vboRoom <- Vbo.fromList GL.Triangles (map (* 40) room) (concat roomNormals)
  vboBall <- Vbo.fromList GL.Points ballVertices ballNormals
  vboPlayer <- Vbo.fromList GL.Points playerVertices playerNormals
  vboCircle <- Vbo.fromList GL.LineStrip (circleVertices 5.0) circleNormals

  let objects = [("player", vboPlayer), ("circle", vboCircle), ("enemy", vboBall), ("room", vboRoom)]

  let shaders = [("default", defaultProgram), ("spherical", sphericalProgram)]

  renderStateRef <- newIORef (RenderState projMatrixArray viewMatrixArray (M.fromList shaders) (M.fromList objects))

  GLFW.keyCallback $= keyboardCallback worldRef
  GLFW.mousePosCallback $= mousePositionCallback worldRef
  GLFW.mouseButtonCallback $= mouseBtnCallback worldRef

  -- invoke the active drawing loop
  mainLoop worldRef renderStateRef renderActions inputAction simulateAction ioActions

  GLFW.closeWindow
  GLFW.terminate

mainLoop :: IORef World -> IORef RenderState -> [RenderAction] -> InputActionPure -> SimulateAction -> IOActions -> IO ()
mainLoop world renderState renderActions inputAction simulateAction ioActions = loop 0.0 world renderState
  where 
 
    loop :: Float -> IORef World -> IORef RenderState -> IO ()
    loop t worldRef renderStateRef = do
      t0 <- getCPUTime

      --updateJoystickState worldRef

      mapM_ (\action -> action t worldRef) ioActions

      modifyIORef worldRef ((inputAction t) . (simulateAction t))

      mapM_ (\action -> action t worldRef renderStateRef) renderActions

      --debugInput worldRef

      GLFW.swapBuffers

      performGC

      t1 <- getCPUTime

      let dt = 0.0166667 -- ((fromIntegral (t1 - t0)) / (10^12)) :: Float

      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
          -- sleep for 1ms to yield CPU to other applications
          GLFW.sleep 0.001

          -- only continue when the window is not closed
          windowOpenStatus <- getParam Opened
          unless (not windowOpenStatus) $
            loop (t + dt) worldRef renderStateRef -- loop with next action


keyboardCallback :: IORef World -> KeyCallback
keyboardCallback worldRef key state = modifyIORef worldRef readKeys  -- >> print key >> print state
  where readKeys world = world { worldInput = input { inputAxisL = axisL } }
          where input = worldInput world
                lx = case key of
                       GLFW.CharKey 'D' -> 1.0
                       GLFW.CharKey 'A' -> (-1.0)
                       otherwise -> 0.0
                ly = case key of
                       GLFW.CharKey 'W' -> 1.0
                       GLFW.CharKey 'S' -> (-1.0)
                       otherwise -> 0.0
                axisL = if state == Press 
                        then [lx,ly,0.0,0.0]
                        else [0.0,0.0,0.0,0.0]

                --lx = if key == GLFW.CharKey 'A' then 1.0 else 0.0
                --ly = if key == GLFW.CharKey 'D' then 1.0 else 0.0
                --rx = if key == GLFW.CharKey 'W' then 1.0 else 0.0
                --ry = if key == GLFW.CharKey 'S' then 1.0 else 0.0

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

debugInput :: IORef World -> IO ()
debugInput worldRef = do world <- readIORef worldRef
                         print (worldInput world)


