module Primitives
    where

import Graphics.Rendering.OpenGL as GL

roomVertices :: [[GLfloat]]
roomVertices = [ [x,y,z] | x <- [(-1.85),1.85], y <- [(-1.0),1.0], z <- [(-1.85),1] ]

roomNormals :: [[GLfloat]]
roomNormals = map (concat.replicate 3) ([ [(1),0,0], [(1),0,0], 
                                          [(-1),0,0], [(-1),0,0], 
                                          [0,1,0], [0,1,0], 
                                          [0,(-1),0], [0,(-1),0],
                                          [0,0,(-1)], [0,0,(-1)] ])
roomIndecies :: [GLuint]
roomIndecies = [ 0,1,2, 1,2,3  -- left
               , 4,5,6, 5,6,7  -- right
               , 0,4,1, 4,5,1  -- floor
               , 2,6,3, 6,7,3 -- ceiling
               , 6,4,0, 6,2,0 -- back
               ]

room :: [GLfloat]
room = concat $ map (\i -> (roomVertices !! (fromEnum i))  ) roomIndecies


ballVertices :: [GLfloat]
ballVertices = concat [ [t,u,v] | t <- [(-6.0)..6.0], u <- [(-6.0)..6.0], v <- [(-6.0)..6.0], u*u + v*v + t*t < 2.0*2.0 ]

ballNormals :: [GLfloat]
ballNormals = concat $ replicate ( length ballVertices ) [0.0, 0.0, 1.0]

playerVertices :: [GLfloat]
playerVertices = concat [ [t,u,v] | t <- [(-6.0)..6.0], u <- [(-6.0)..6.0], v <- [(-6.0)..6.0], 2*u*u + 0.5*u*v + 2*v*v + t < 8.0 ]

playerNormals :: [GLfloat]
playerNormals = concat $ replicate ( length playerVertices ) [0.0, 0.0, 1.0]


circleVertices :: GLfloat -> [GLfloat]
circleVertices r = concat [ [ (1)*r * sin t, r * cos t, 0.0] | t <- [0, 2*pi/24.0 .. 2*pi] ]

circleNormals :: [GLfloat]
circleNormals = concat [ [ 0.0, 0.0, 1.0] | t <- [0, 2*pi/24.0 .. 2*pi] ]
