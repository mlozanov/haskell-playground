module Vbo where

import Graphics.Rendering.OpenGL 

data Vbo = Vbo { buffer :: GLint }

newVbo :: IO Vbo
newVbo = return $ Vbo 0

withVbo :: Vbo -> IO ()
withVbo vbo = return ()

fromList :: [GLfloat] -> Vbo
fromList l = Vbo 0

