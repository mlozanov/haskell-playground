module Shader where

import Control.Monad
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT as GLUT (reportErrors)

data ShaderProgram = SP { vs :: String
                        , fs :: String
                        , gs :: String -- not supported yet
                        , program :: Program
                        }

newProgram :: String -> String -> IO ShaderProgram
newProgram v f = do vs <- readAndCompileShader v
                    fs <- readAndCompileShader f
                    p <- linkShaders [vs] [fs]
                    return $ SP v f "" p

withProgram :: ShaderProgram -> IO () -> IO ()
withProgram s a = do currentProgram $= Just (program s)
                     a
                     currentProgram $= Nothing

getUniformLocation p s = GL.get $ uniformLocation (program p) s 

readAndCompileShader :: Shader s => FilePath -> IO s
readAndCompileShader filePath = do
  src <- readFile filePath
  [shader] <- genObjectNames 1
  shaderSource shader $= [src]
  compileShader shader
  reportErrors
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)
  mapM_ putStrLn ["Shader >" ++ filePath ++ ":", infoLog ]
  unless ok $ do 
    deleteObjectNames [shader]
    ioError (userError "compilation failed")
  return shader

linkShaders :: [VertexShader] -> [FragmentShader] -> IO Program
linkShaders vs fs = do
  [prog] <- genObjectNames 1
  attachedShaders prog $=  (vs, fs)
  linkProgram prog
  reportErrors
  ok <- get (linkStatus prog)
  infoLog <- get (programInfoLog prog)
  mapM_ putStrLn ["Program >", infoLog]
  unless ok $ do deleteObjectNames [prog]
                 ioError (userError "linking failed")  
  return prog

