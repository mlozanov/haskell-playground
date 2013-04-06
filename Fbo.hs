module Fbo where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31 as Core

import Foreign

data Fbo = Fbo { fboBuffers :: FramebufferObject
               , textureObject :: TextureObject
               , fboWidth :: GLuint
               , fboHeight :: GLuint
               }

fbo :: GLuint -> GLuint -> IO Fbo
fbo width height  = do [textureObject] <- genObjectNames 1
                       textureBinding Texture2D $= Just textureObject
                       --texture Texture2D $= Enabled
                       texImage2D Nothing NoProxy 0 RGBA8 (TextureSize2D 1280 720) 0 (PixelData BGRA UnsignedByte (plusPtr nullPtr 0))

                       [b1] <- genObjectNames 1
                       bindFramebuffer Framebuffer $= b1
                       framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing textureObject 0

                       bindFramebuffer Framebuffer $= defaultFramebufferObject

                       return $ Fbo b1 textureObject width height

withFbo :: Fbo -> IO () -> IO ()
withFbo b actions = do
  bindFramebuffer Framebuffer $= (fboBuffers b) 
  --viewport $= (GL.Position 0 0, GL.Size 512 512)
  actions
  bindFramebuffer Framebuffer $= defaultFramebufferObject

addAttachment :: Fbo -> IO Fbo
addAttachment o = return o
