module Actor where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT.Objects as O

import Graphics

data Actor = SimpleActor
           | Actor [Double]
             deriving (Eq, Ord, Show)

type Actors = [Actor]

draw :: Actor -> IO ()
draw SimpleActor = do
  GL.color $ color3 1 1 0
  O.renderObject O.Solid (O.Cube 2.0)

draw (Actor actor) = preservingMatrix $ do
  GL.color $ color3 1 0 0
  O.renderObject O.Solid (O.Cube 1.5)
  GL.translate $ vector3 0 (-1.0) 0
  O.renderObject O.Solid (O.Cube 1.3)


