module Collision where

import Math

collision :: Shape a -> Shape a -> Bool
collision (Circle p1 r1) (Circle p2 r2) = (r1 + r2) < lengthVec (subVec r1 r2) 
collision s1 s2 = undefined

