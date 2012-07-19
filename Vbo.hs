module Vbo where

import Graphics.Rendering.OpenGL 
import Data.Array.MArray
import Data.Array.Storable
import Foreign
import Foreign.Storable

data Vbo = Vbo { buffer :: BufferObject 
               , bufferSize :: GLuint
               , vertexBufferSize :: GLuint
               , normalBufferSize :: GLuint
               , primitiveMode :: PrimitiveMode
               }
         | IndexVbo { vertexBuffer :: BufferObject
                    , indexBuffer :: BufferObject
                    , vertexBufferSize :: GLuint
                    , indexBufferSize :: GLuint
                    }

fromList :: PrimitiveMode -> [GLfloat] -> [GLfloat] -> IO Vbo
fromList mode vs ns = 
    do [buffer] <- genObjectNames 1
       arr <- newListArray (0, vsize - 1) elems
                        
       bindBuffer ArrayBuffer $= Just buffer                     
       withStorableArray arr (\ptr -> bufferData ArrayBuffer $= (toEnum vsize, ptr, StaticDraw))
       bindBuffer ArrayBuffer $= Nothing

       return $ Vbo buffer (toEnum vsize) vsLength nsLength mode
           where elems = vs ++ ns
                 vsize = 4 * length elems
                 vsLength = toEnum $ length vs
                 nsLength = toEnum $ length ns


renderVbo :: Vbo -> IO ()
renderVbo (Vbo vs vsize vssize nssize mode) = do
  bindBuffer ArrayBuffer $= Just vs
  arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 offset0
  arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 (offset (fromIntegral (4 * vssize)))
  clientState VertexArray $= Enabled
  clientState NormalArray $= Enabled

  drawArrays mode 0 $ (toEnum . fromEnum) vsize

  clientState VertexArray $= Disabled
  clientState NormalArray $= Disabled
  bindBuffer ArrayBuffer $= Nothing

renderVbo vbo@(IndexVbo v i vsize _) = do 
  bindBuffer ArrayBuffer $= Just v
  arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 offset0
  arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 (offset 288)
  clientState VertexArray $= Enabled
  clientState NormalArray $= Enabled

  bindBuffer ElementArrayBuffer $= Just i
  clientState IndexArray $= Enabled
  drawElements Triangles ((toEnum . fromEnum) vsize) UnsignedInt offset0

  clientState VertexArray $= Disabled
  clientState NormalArray $= Disabled
  clientState IndexArray $= Disabled

offset a = plusPtr nullPtr a
offset0 = offset 0
