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
                       texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D 1280 720) 0 (PixelData BGR Float (plusPtr nullPtr 0))
                       textureFilter Texture2D $= ((Linear', Nothing), Linear')
                       --textureWrapMode Texture2D S $= (Repeated, Clamp)
                       --textureWrapMode Texture2D T $= (Repeated, Clamp)
                       textureBinding Texture2D $= Nothing

                       [b1] <- genObjectNames 1

                       [rb] <- genObjectNames 1

                       bindRenderbuffer Renderbuffer $= rb
                       renderbufferStorage Renderbuffer DepthComponent24 (RenderbufferSize 1280 720)

                       bindFramebuffer Framebuffer $= b1
                       framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing textureObject 0
                       --framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rb

                       bindFramebuffer Framebuffer $= defaultFramebufferObject

                       return $ Fbo b1 textureObject width height

withFbo :: Fbo -> IO () -> IO ()
withFbo b actions = do
  bindFramebuffer Framebuffer $= (fboBuffers b) 
  actions
  bindFramebuffer Framebuffer $= defaultFramebufferObject

addAttachment :: Fbo -> IO Fbo
addAttachment o = return o
