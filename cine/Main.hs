{-# LANGUAGE RankNTypes #-}

module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT.Objects as O
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Concurrent

data Action = Action (IO Action)

data Camera = EmptyCamera
            | Camera [Double]
              deriving (Eq, Ord, Show)

data Actor = SimpleActor
           | Actor [Double]
             deriving (Eq, Ord, Show)

type Cameras = [Camera]
type Actors = [Actor]

type Vec4 = [Float]
type Vec3 = [Float]
type Matrix44 = [Vec4]

data World = World { cameras :: Cameras
                   , actors :: Actors }


addVec :: Vec4 -> Vec4 -> Vec4
addVec = zipWith (+)

subVec :: Vec4 -> Vec4 -> Vec4
subVec = zipWith (-)

innerVec :: Vec4 -> Vec4 -> Vec4
innerVec = zipWith (*)

negateVec :: Vec4 -> Vec4
negateVec = map negate

dotVec :: Vec4 -> Vec4 -> Float
dotVec a b = sum $ innerVec a b

lengthVec :: Vec4 -> Float
lengthVec a = sqrt . sum $ map square a
    where square x = x*x

degToRad a = a * pi / 180.0
radToDeg a = a * 180.0 / pi


cameraFixed :: Camera -> State Actors Camera
cameraFixed camera = return camera

cameraOrbit :: Camera -> State Actors Camera
cameraOrbit camera = return camera

cameraDamp :: Camera -> State Actors Camera
cameraDamp camera = return camera

cameraFrameActors :: Camera -> State Actors Camera
cameraFrameActors camera = return camera

simpleFraming camera = cameraOrbit camera >>= cameraDamp >>= cameraFrameActors

main = do 
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 960 640) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
       do GL.viewport   $= (GL.Position 0 0, size)
          GL.matrixMode $= GL.Projection
          GL.loadIdentity
          --GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
          GL.perspective (degToRad 45.0) (realToFrac w/realToFrac h) 0.1 1000.0
 
  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  cameras <- newIORef []
  state <- newIORef [SimpleActor, SimpleActor]

  GL.get GL.vendor >>= print
  GL.get GL.renderer >>= print
  GL.get GL.glVersion >>= print
  GL.get GL.shadingLanguageVersion >>= print
  
  -- invoke the active drawing loop
  let w = World [EmptyCamera] [SimpleActor]
  update w renderer'

  --updateCameras cameras state

  -- finish up
  GLFW.closeWindow
  GLFW.terminate

updateCameras cameras state = loop 
  where
    loop = do
      return $ evalState ((simpleFraming . head) cameras) state
      --loop cameras state

-- we start with waitForPress action
update world render = loop waitForPress
  where 
 
    loop action = do
      render world

      -- swap buffer
      GLFW.swapBuffers
      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
          -- perform action
          Action action' <- action
          -- sleep for 1ms to yield CPU to other applications
          GLFW.sleep 0.001

          -- only continue when the window is not closed
          windowOpenStatus <- getParam Opened
          unless (not windowOpenStatus) $
            loop action' -- loop with next action

    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          --modifyIORef lines (((x,y):) . ((x,y):))
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        --modifyIORef lines (((x,y):) . tail)
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)

renderer' world = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  GL.translate (GL.Vector3 0.0 0.0 (-200.0 :: GLfloat))
  GL.rotate (35.0::GLfloat)  (GL.Vector3 0 1 0)

  GL.color $ color3 1 1 1
  O.renderObject O.Solid (O.Teapot 1.0) 

 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3


simulate :: forall model . model -> model
simulate w = w

