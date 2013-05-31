module Fbo where

import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw.Core31 as Core

import Foreign

data Fbo = Fbo { fboBuffers :: FramebufferObject
               , textureObject :: [TextureObject]
               , fboWidth :: GLuint
               , fboHeight :: GLuint
               }

albedoTarget fbo = albedo
  where [albedo,lighting,bloom] = textureObject fbo

lightingTarget fbo = lighting
  where [albedo,lighting,bloom] = textureObject fbo

bloomTarget fbo = bloom
  where [albedo,lighting,bloom] = textureObject fbo

fbo :: GLuint -> GLuint -> IO Fbo
fbo width height  = do [albedo,lighting,bloom] <- genObjectNames 3
                       textureBinding Texture2D $= Just albedo
                       texImage2D Nothing NoProxy 0 RGB32F (TextureSize2D 1280 720) 0 (PixelData RGB Float (plusPtr nullPtr 0))
                       textureFilter Texture2D $= ((Linear', Nothing), Linear')
                       textureBinding Texture2D $= Nothing

                       textureBinding Texture2D $= Just lighting
                       texImage2D Nothing NoProxy 0 R32F (TextureSize2D 1280 720) 0 (PixelData Red Float (plusPtr nullPtr 0))
                       textureFilter Texture2D $= ((Linear', Nothing), Linear')
                       textureBinding Texture2D $= Nothing

                       textureBinding Texture2D $= Just bloom
                       texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D 1280 720) 0 (PixelData RGB UnsignedByte (plusPtr nullPtr 0))
                       textureFilter Texture2D $= ((Linear', Nothing), Linear')
                       textureBinding Texture2D $= Nothing

                       [b1] <- genObjectNames 1

                       [rb] <- genObjectNames 1

                       bindRenderbuffer Renderbuffer $= rb
                       renderbufferStorage Renderbuffer DepthComponent24 (RenderbufferSize 1280 720)

                       bindFramebuffer Framebuffer $= b1
                       framebufferTexture2D Framebuffer (ColorAttachment 0) Nothing albedo 0
                       framebufferTexture2D Framebuffer (ColorAttachment 1) Nothing lighting 0
                       framebufferTexture2D Framebuffer (ColorAttachment 2) Nothing bloom 0
                       framebufferRenderbuffer Framebuffer DepthAttachment Renderbuffer rb

                       bindFramebuffer Framebuffer $= defaultFramebufferObject

                       return $ Fbo b1 [albedo,lighting,bloom] width height

withFbo :: Fbo -> IO () -> IO ()
withFbo b actions = do
  bindFramebuffer Framebuffer $= (fboBuffers b) 
  actions
  bindFramebuffer Framebuffer $= defaultFramebufferObject

addAttachment :: Fbo -> IO Fbo
addAttachment o = return o
