module Collision where

import Math

collide :: (Floating a, Ord a) => (Shape a, Shape a) -> Bool
collide ((Circle p1 r1), (Circle p2 r2)) = (r1 + r2) < lengthVec (subVec p1 p2) 
collide ((Circle cp cr), (Rectangle r1 r2 r3 r4)) = False
collide (s1, s2) = False

