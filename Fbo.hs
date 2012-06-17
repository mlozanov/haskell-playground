module Fbo where

import Graphics.Rendering.OpenGL 

data Fbo = Fbo { buffer :: GLint }

newFbo :: IO Fbo
newFbo = return $ Fbo 0

withFbo :: Fbo -> IO ()
withFbo fbo = return ()

fromList :: [GLfloat] -> Fbo
fromList l = Fbo 0

