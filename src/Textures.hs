{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Textures (texturedRectangle) where

import Text.RawString.QQ

import qualified Graphics.UI.GLFW as GLFW
import qualified GLWrap as GL
import qualified Data.ByteString as B

import Behaviour

texturedRectangle :: IO (IO (), IO ())
texturedRectangle = do
  [vbo, ebo] <- GL.genBuffers 2
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5,  0,     1, 0, 0,   2, 2
    ,  0.5, -0.5,  0,     0, 1, 0,   2, 0
    , -0.5, -0.5,  0,     0, 0, 1,   0, 0
    , -0.5,  0.5,  0,     1, 1, 0,   0, 2
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
      void main() {
        gl_Position = vec4(position, 1.0f);
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
      void main() {
        color = mix(texture(ourTexture1, TexCoord), texture(ourTexture2, TexCoord), 0.2);
      }
      |]

  let render = do
        GL.useProgram prog

        GL.activeTexture GL.Texture0
        GL.bindTexture GL.Texture2D texCont
        loc1 <- GL.getUniformLocation prog "ourTexture1"
        GL.uniform1i loc1 0

        GL.activeTexture GL.Texture1
        GL.bindTexture GL.Texture2D texFace
        loc2 <- GL.getUniformLocation prog "ourTexture2"
        GL.uniform1i loc2 1

        GL.bindVertexArray vao
        GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo, ebo]
        GL.deleteProgram prog
        GL.deleteTextures [texCont, texFace]
        return ()

  return (render, cleanup)
