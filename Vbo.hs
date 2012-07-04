module Vbo where

import Graphics.Rendering.OpenGL 
import Data.Array.MArray
import Data.Array.Storable
import Foreign

data Vbo = Vbo { vertexBuffer :: BufferObject
               , indexBuffer :: BufferObject
               , vertexBufferSize :: GLuint
               , indexBufferSize :: GLuint

--               , primitiveMode :: PrimitiveMode
--               , vertexCount :: GLuint
--               , normalCount :: GLuint
               }

newVbo :: IO Vbo
newVbo = do [v] <- genObjectNames 1
            [i] <- genObjectNames 1
            return $ Vbo v i 0 0

withVertexBuffer :: Vbo -> IO ()
withVertexBuffer vbo = bindBuffer ArrayBuffer $= Just (vertexBuffer vbo)

withIndexBuffer :: Vbo -> IO ()
withIndexBuffer vbo = bindBuffer ElementArrayBuffer $= Just (indexBuffer vbo)

fromList :: [GLfloat] -> [GLuint] -> IO Vbo
fromList v i = do 
  vbo <- newVbo

  withVertexBuffer vbo
  let vsize = 4 * length v
  print vsize
  
  arr <- newListArray (0, vsize - 1) v 
  withStorableArray arr (\ptr -> bufferData ArrayBuffer $= (toEnum vsize, ptr, StaticDraw))

  withIndexBuffer vbo
  let isize = length i
  print isize
  arri <- newListArray (0, isize - 1) i
  withStorableArray arri (\ptr -> bufferData ElementArrayBuffer $= (toEnum (4*isize), ptr, StaticDraw))

  return vbo { vertexBufferSize = toEnum vsize, indexBufferSize = toEnum isize }

renderVbo :: Vbo -> IO ()
renderVbo vbo = do 
  withVertexBuffer vbo
  arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 offset0
  arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 (offset 288)
  clientState VertexArray $= Enabled
  clientState NormalArray $= Enabled

  withIndexBuffer vbo
  clientState IndexArray $= Enabled
  drawElements Triangles ((toEnum . fromEnum) (indexBufferSize vbo)) UnsignedInt offset0

  clientState VertexArray $= Disabled
  clientState NormalArray $= Disabled
  clientState IndexArray $= Disabled

offset a = plusPtr nullPtr a
offset0 = offset 0
