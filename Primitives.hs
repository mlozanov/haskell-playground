module Primitives (room, roomNormals, ball)
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
ballVertices = [ [x,y,z] | x <- [(-40.0),(-36.0) .. 40.0], y <- [(-40.0),(-36.0) .. 40.0], z <- [(-40.0),(-36.0) .. 40.0]]

ball :: [GLfloat]
ball = concat ballVertices




