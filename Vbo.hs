module Vbo where

import Graphics.Rendering.OpenGL 
import Data.Array.MArray
import Data.Array.Storable
import Foreign

data Vbo = Vbo { bufferObject :: BufferObject
               , bufferSize :: GLint 
               }

newVbo :: IO Vbo
newVbo = do [array] <- genObjectNames 1
            bindBuffer ArrayBuffer $= Just array
            bindBuffer ArrayBuffer $= Nothing
            --reportErrors
            return $ Vbo array 0

withVbo :: IO () -> Vbo -> IO ()
withVbo actions vbo = bindBuffer ArrayBuffer $= Just (bufferObject vbo) >> actions >> bindBuffer ArrayBuffer $= Nothing >> return ()

fromList :: [GLfloat] -> IO Vbo
fromList l = do vbo <- newVbo
                let size = 4 * length l
                let f l = do arr <- newListArray (0, size - 1) l
                             withStorableArray arr (\ptr -> bufferData ArrayBuffer $= (toEnum size, ptr, StaticDraw))
                             return ()

                withVbo (f l) vbo
                return vbo { bufferSize = toEnum size }


drawVbo :: Vbo -> IO ()
drawVbo vbo = do arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 (offset 0)
                 --arrayPointer ColorArray $= VertexArrayDescriptor 4 Float 28 (offset 12)
                 clientState VertexArray $= Enabled
                 --clientState ColorArray $= Enabled

                 drawArrays LineStrip 0 $ (toEnum . fromEnum) (bufferSize vbo)
    where offset a = plusPtr nullPtr a

