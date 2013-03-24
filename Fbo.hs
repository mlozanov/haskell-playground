module Fbo where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31 as Core

data Fbo = Fbo { fboBuffers :: [FramebufferObject]
               , fboWidth :: GLuint
               , fboHeight :: GLuint
               }

fbo :: GLuint -> GLuint -> IO Fbo
fbo width height  = do [resolveBuffer] <- genObjectNames 2
                       --glBindRenderbuffer gl_
                       return $ Fbo [resolveBuffer] width height

withFbo :: Fbo -> IO () -> IO ()
withFbo fbo actions = actions >> return ()

