module Fbo where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31 as Core

data Fbo = Fbo { fboBuffer :: BufferObject
               , textureBuffer :: BufferObject
               , renderBuffer :: BufferObject
               , fboWidth :: GLuint
               , fboHeight :: GLuint
               }

newFbo :: GLuint -> GLuint -> IO Fbo
newFbo width height  = do [f,t,r] <- genObjectNames 3
                                          
                          --glBindFramebuffer gl_FRAMEBUFFER f

                          return $ Fbo f t r width height

withFbo :: Fbo -> IO () -> IO ()
withFbo fbo actions = actions >> return ()

