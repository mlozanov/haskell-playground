{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shader where

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLUT as GLUT (reportErrors)

data ShaderProgramData = SP { vs :: String
                            , fs :: String
                            , gs :: String -- not supported yet
                            , program :: GL.Program
                            }

newProgram :: String -> String -> IO ShaderProgramData
newProgram v f = do vs <- readAndCompileShader GL.VertexShader v
                    fs <- readAndCompileShader GL.FragmentShader f
                    p <- linkShaders [vs] [fs]
                    return $ SP v f "" p

newtype ShaderProgram a = ShaderProgram (StateT ShaderProgramData IO a)
  deriving (Monad, MonadIO, MonadState ShaderProgramData, MonadPlus, Alternative, Functor, Applicative)

withProgram :: ShaderProgramData -> IO () -> IO ()
withProgram s a = do GL.currentProgram GL.$= Just (program s)
                     a
                     GL.currentProgram GL.$= Nothing

getUniformLocation p s = GL.get $ GL.uniformLocation (program p) s

readAndCompileShader :: GL.ShaderType -> FilePath -> IO GL.Shader
readAndCompileShader shaderType filePath = do
  src <- readFile filePath
  --[shader] <- GL.genObjectNames 1
  shader <- GL.createShader shaderType
  GL.shaderSource shader GL.$= [src]
  GL.compileShader shader
  GLUT.reportErrors
  ok <- GL.get (GL.compileStatus shader)
  infoLog <- GL.get (GL.shaderInfoLog shader)
  mapM_ putStrLn ["Shader >" ++ filePath ++ ":", infoLog ]
  unless ok $ do
    GL.deleteObjectNames [shader]
    ioError (userError "compilation failed")
  return shader

linkShaders :: [GL.Shader] -> [GL.Shader] -> IO GL.Program
linkShaders vs fs = do
  --[prog] <- GL.genObjectNames 1
  prog <- GL.createProgram
  GL.attachedShaders prog GL.$= vs ++ fs
  GL.linkProgram prog
  GLUT.reportErrors
  ok <- GL.get (GL.linkStatus prog)
  infoLog <- GL.get (GL.programInfoLog prog)
  mapM_ putStrLn ["Program >", infoLog]
  unless ok $ do GL.deleteObjectNames [prog]
                 ioError (userError "linking failed")
  return prog
