{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Textures (texturedRectangle) where

import Text.RawString.QQ

import qualified Graphics.UI.GLFW as GLFW
import qualified GLWrap as GL
import qualified Data.ByteString as B

import Linear.Matrix
import Linear.Quaternion (axisAngle)
import Data.Distributive (distribute)
import Linear.Vector
import Linear.V3
import Linear.V4
import Control.Lens

import Behaviour

data AppState = AppState { mix :: !Float
                         , vbo :: GL.Buffer
                         , ebo :: GL.Buffer
                         , vao :: GL.VertexArray
                         , texCont :: GL.Texture
                         , texFace :: GL.Texture
                         , prog :: GL.Program
                         , transMat :: M44 Float
                         , transMat2 :: M44 Float
                         }

texturedRectangle = mkBehaviour initialize frame render cleanup

initialize :: IO AppState
initialize = do
  [vbo, ebo] <- GL.genBuffers 2
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5,  0,     1, 0, 0,   1, 1
    ,  0.5, -0.5,  0,     0, 1, 0,   1, 0
    , -0.5, -0.5,  0,     0, 0, 1,   0, 0
    , -0.5,  0.5,  0,     1, 1, 0,   0, 1
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 32 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 3 GL.AttribPointerFloat False 32 12
  GL.enableVertexAttribArray 1

  GL.vertexAttribPointer 2 2 GL.AttribPointerFloat False 32 24
  GL.enableVertexAttribArray 2

  GL.bindBuffer GL.TargetElementArray ebo
  GL.uintBufferData GL.TargetElementArray GL.UsageStaticDraw
    [ 0, 1, 2
    , 0, 2, 3
    ]

  GL.unbindVertexArray

  [texCont, texFace] <- GL.genTextures 2

  GL.bindTexture GL.Texture2D texCont
  GL.texParameter GL.Texture2D (GL.TextureWrapS GL.ClampToEdge)
  GL.texParameter GL.Texture2D (GL.TextureWrapT GL.ClampToEdge)
  GL.texParameter GL.Texture2D (GL.TextureMinFilter GL.MinLinear)
  GL.texParameter GL.Texture2D (GL.TextureMagFilter GL.MagLinear)
  GL.texImage2D "container.jpg"
  GL.generateMipmap GL.Texture2D
  GL.unbindTexture GL.Texture2D

  GL.bindTexture GL.Texture2D texFace
  GL.texParameter GL.Texture2D (GL.TextureWrapS GL.Repeat)
  GL.texParameter GL.Texture2D (GL.TextureWrapT GL.Repeat)
  GL.texParameter GL.Texture2D (GL.TextureMinFilter GL.MinLinear)
  GL.texParameter GL.Texture2D (GL.TextureMagFilter GL.MagLinear)
  GL.texImage2D "awesomeface.png"
  GL.generateMipmap GL.Texture2D
  GL.unbindTexture GL.Texture2D

  prog <- GL.stdProgram
    [r|#version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec3 color;
      layout (location = 2) in vec2 texCoord;
      out vec3 ourColor;
      out vec2 TexCoord;
      uniform mat4 transform;
      void main() {
        gl_Position = transform * vec4(position, 1.0f);
        ourColor = color;
        TexCoord = vec2(texCoord.x, 1.0f-texCoord.y);
      }
      |]
    [r|#version 330 core
      in vec3 ourColor;
      in vec2 TexCoord;
      out vec4 color;
      uniform sampler2D ourTexture1;
      uniform sampler2D ourTexture2;
      uniform float mixCoeff;
      void main() {
        color = mix(texture(ourTexture1, TexCoord), texture(ourTexture2, TexCoord), mixCoeff);
      }
      |]

  let mix = 0.5
  let transMat = identity
  let transMat2 = identity
  return $ AppState{..}


handleInput :: InputEvent -> AppState -> AppState
handleInput (KeyEvent GLFW.Key'A _ GLFW.KeyState'Released _) st = st { mix = rotateMix }
  where rotateMix = clamp $ mix st + 0.1
        clamp mix
          | mix > 1.0 = 0
          | otherwise = mix
handleInput _ st = st

frame :: [InputEvent] -> Float -> AppState -> AppState
frame events t st = calcTransform t (applyEvents events st)
  where
    applyEvents events st = foldr handleInput st events
    calcTransform t st = st { transMat = xlate !*! rot
                            , transMat2 = xlate2 !*! scale
                            }
      where xlate :: M44 Float = identity & translation .~ V3 0.5 (-0.5) 0
            xlate2 = identity & translation .~ V3 (-0.5) 0.5 0
            rot :: M44 Float = m33_to_m44 $ fromQuaternion (axisAngle (V3 0 0 1) (t * pi / 4))
            scaleScalar = abs (sin t)
            scale :: M44 Float = scaled (V4 scaleScalar scaleScalar scaleScalar 1)



render :: AppState -> GL.Width -> GL.Height -> IO ()
render AppState{..} _ _ = do
  transLoc <- GL.getUniformLocation prog "transform"

  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor]

  GL.useProgram prog

  GL.activeTexture GL.Texture0
  GL.bindTexture GL.Texture2D texCont
  loc1 <- GL.getUniformLocation prog "ourTexture1"
  GL.uniform1i loc1 0

  GL.activeTexture GL.Texture1
  GL.bindTexture GL.Texture2D texFace
  loc2 <- GL.getUniformLocation prog "ourTexture2"
  GL.uniform1i loc2 1

  locMix <- GL.getUniformLocation prog "mixCoeff"
  GL.uniform1f locMix $ mix

  GL.bindVertexArray vao

  GL.uniformMatrix4fv transLoc transMat
  GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint

  GL.uniformMatrix4fv transLoc transMat2
  GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint

  GL.unbindVertexArray

cleanup :: AppState -> IO ()
cleanup AppState{..} = do
  GL.deleteVertexArrays [vao]
  GL.deleteBuffers [vbo, ebo]
  GL.deleteProgram prog
  GL.deleteTextures [texCont, texFace]
