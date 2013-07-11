module Vbo where

import Graphics.Rendering.OpenGL as GL
import Data.Array.MArray
import Data.Array.Storable
import Foreign
import Foreign.Storable

data Vbo = Vbo { bufferArray :: VertexArrayObject
               , buffer :: BufferObject 
               , bufferSize :: GLuint
               , vertexBufferSize :: GLuint
               , normalBufferSize :: GLuint
               , primitiveMode :: PrimitiveMode
               }
         | IndexVbo { ivboBufferArray :: VertexArrayObject
                    , ivboPrimitiveMode :: PrimitiveMode
                    , ivboElementBuffer :: BufferObject
                    , ivboIndexBuffer :: BufferObject
                    , ivboElementBufferSize :: GLuint
                    , ivboIndexBufferSize :: GLuint
                    }

fromList :: PrimitiveMode -> [GLfloat] -> [GLfloat] -> IO Vbo
fromList mode vs ns = do
  [buffer] <- genObjectNames 1
  [ba] <- genObjectNames 1

  arr <- newListArray (0, vsize - 1) elems
                  
  bindVertexArrayObject $= Just ba
  --GL.get GL.errors >>= print

  bindBuffer ArrayBuffer $= Just buffer                     
  withStorableArray arr (\ptr -> bufferData ArrayBuffer $= (toEnum vsize, ptr, StaticDraw))
  --GL.get GL.errors >>= print

  vertexAttribPointer (AttribLocation 0) $= (ToFloat, (VertexArrayDescriptor 3 Float 0 (offset 0)))
  --GL.get GL.errors >>= print
  vertexAttribArray (AttribLocation 0) $= Enabled
  --GL.get GL.errors >>= print

  vertexAttribPointer (AttribLocation 1) $= (ToFloat, (VertexArrayDescriptor 3 Float 0 (offset (fromIntegral (4 * vsLength)))))
  --GL.get GL.errors >>= print
  vertexAttribArray (AttribLocation 1) $= Enabled
  --GL.get GL.errors >>= print

  bindVertexArrayObject $= Nothing
  --GL.get GL.errors >>= print

  return $ Vbo ba buffer (toEnum vsize) vsLength nsLength mode
     where elems = vs ++ ns
           vsize = 4 * length elems
           vsLength = toEnum $ length vs
           nsLength = toEnum $ length ns

fromList' :: PrimitiveMode -> [GLfloat] -> [GLuint] -> IO Vbo
fromList' mode vs is = do
  [vbuffer] <- genObjectNames 1
  [ibuffer] <- genObjectNames 1
  [ba] <- genObjectNames 1

  bindVertexArrayObject $= Just ba

  varr <- newListArray (0, varrSize - 1) vs
  iarr <- newListArray (0, iarrSize - 1) is

  bindBuffer ArrayBuffer $= Just vbuffer
  withStorableArray varr (\ptr -> bufferData ArrayBuffer $= (toEnum varrSize, ptr, StaticDraw))

  bindBuffer ElementArrayBuffer $= Just ibuffer
  withStorableArray iarr (\ptr -> bufferData ElementArrayBuffer $= (toEnum iarrSize, ptr, StaticDraw))

  vertexAttribPointer (AttribLocation 0) $= (ToFloat, (VertexArrayDescriptor 3 Float 24) (offset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  vertexAttribPointer (AttribLocation 1) $= (ToFloat, (VertexArrayDescriptor 3 Float 24) (offset 12))
  vertexAttribArray (AttribLocation 1) $= Enabled

  --bindBuffer ArrayBuffer $= Nothing
  --bindBuffer ElementArrayBuffer $= Nothing
  bindVertexArrayObject $= Nothing

  return $ IndexVbo ba mode vbuffer ibuffer vssize issize
    where vssize = f vs
          issize = f is
          varrSize = 4 * length vs
          iarrSize = 4 * length is
          f = toEnum . length


renderVbo :: Vbo -> IO ()
renderVbo (Vbo va vs vsize vssize nssize mode) = do
  bindVertexArrayObject $= Just va
  drawArrays mode 0 (convertType vsize)
  bindVertexArrayObject $= Nothing

renderVbo vbo@(IndexVbo va mode elements indices essize issize) = do 
  bindVertexArrayObject $= Just va
  bindBuffer ElementArrayBuffer $= Just indices
  drawElements mode (convertType issize) UnsignedInt (offset 0)
  bindVertexArrayObject $= Nothing


-- helpers
offset a = plusPtr nullPtr a

convertType = toEnum . fromEnum