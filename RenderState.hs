module RenderState where

import Foreign.Ptr
import Foreign.Marshal.Array

import Data.IORef
import qualified Data.Map as M

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.UI.GLUT.Objects as O
import Graphics.Rendering.OpenGL (($=))

import Shader
import Vbo
import Fbo

data RenderState = RenderState { projectionMatrix :: Ptr GLfloat
                               , viewMatrix :: Ptr GLfloat
                               , modelMatrix :: Ptr GLfloat
                               , shaderProgramsMap :: M.Map String ShaderProgramData
                               , vboMap :: M.Map String Vbo
                               , fboMap :: M.Map String Fbo
                               }
