module Fbo where

import Graphics.Rendering.OpenGL 

data Fbo = Fbo { buffer :: GLint 
               , buferSize :: GLint
               }

newFbo :: IO Fbo
newFbo = return $ Fbo 0 0

withFbo :: Fbo -> IO () -> IO ()
withFbo fbo actions = actions >> return ()

fromList :: [GLfloat] -> IO Fbo
fromList l = return $ Fbo 0 0

