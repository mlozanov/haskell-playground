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


ballVertices :: [[GLfloat]]
--ballVertices = [ [x,y,z] | x <- [(-10.0),(-9.0) .. 10.0], y <- [(-10.0),(-9.0) .. 10.0], z <- [(-10.0),(-9.0) .. 10.0], x*x + y*y + z*z < 10.0*10.0 ]
ballVertices = [ [t,u,v] | t <- [(-6.0)..6.0], u <- [(-6.0)..6.0], v <- [(-6.0)..6.0], 2*u*u + 0.5*u*v + 2*v*v + t < 8.0 ]

ballNormals :: [[GLfloat]]
ballNormals = replicate ( length ballVertices ) [0.0, 0.0, 1.0]

ball :: [GLfloat]
ball = concat ballVertices

--tri :: [GLfloat]
--tri = []


