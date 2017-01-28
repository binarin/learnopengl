{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Hello
  ( triangleWithPerVertexColor
  , triangleColorFromUniform
  , triangleUsingArray
  , doubleTrianglesArray
  , doubleTrianglesDifferentArrays
  , doubleTrianglesDifferentColors
  , rectangleUsingElements
  ) where

import Text.RawString.QQ

import qualified Graphics.UI.GLFW as GLFW
import qualified GLWrap as GL
import qualified Data.ByteString as B


triangleWithPerVertexColor :: IO (IO (), IO ())
triangleWithPerVertexColor = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0, 1, 0, 0
    , -0.5, -0.5, 0.0, 0, 1, 0
    ,    0,  0.5, 0.0, 0, 0, 1
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 24 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 3 GL.AttribPointerFloat False 24 12
  GL.enableVertexAttribArray 1
  GL.unbindVertexArray

  prog <- GL.stdProgram
    [r|#version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec3 color;
      out vec3 ourColor;
      uniform float xOffset;
      void main() {
        gl_Position = vec4(position.x + xOffset, -position.y, position.z, 1.0);
        ourColor = color;
      }
      |]
    [r|#version 330 core
      in vec3 ourColor;
      out vec4 color;
      void main() {
        color = vec4(ourColor, 1.0);
      }
      |]

  offsetLocation <- GL.getUniformLocation prog "xOffset"

  let render = do
        GL.useProgram prog
        GL.uniform1f offsetLocation (0.3 :: Float)
        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)


triangleColorFromUniform :: IO (IO (), IO ())
triangleColorFromUniform = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- GL.stdProgram vertexShaderSrc
    [r|#version 330 core
      out vec4 color;
      uniform vec4 ourColor;
      void main() {
        color = ourColor;
      }
      |]

  let calcGreenValue = do
        maybeTime <- GLFW.getTime
        case maybeTime of
          Nothing -> return 0
          Just time -> return $ (sin(time) / 2) + 0.5

  vertexColorLocation <- GL.getUniformLocation prog "ourColor"

  let render = do
        GL.useProgram prog
        greenValue <- calcGreenValue
        GL.uniform4f vertexColorLocation 0 greenValue 0 1

        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)


triangleUsingArray :: IO (IO (), IO ())
triangleUsingArray = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- GL.stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)

doubleTrianglesArray :: IO (IO (), IO ())
doubleTrianglesArray = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ,  0.7,  0.7, 0.0
    ,  0.8,  0.7, 0.0
    ,  0.8,  0.6, 0.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- GL.stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 6
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)


doubleTrianglesDifferentArrays :: IO (IO (), IO ())
doubleTrianglesDifferentArrays = do
  [vao1, vao2] <- GL.genVertexArrays 2
  [vbo1, vbo2] <- GL.genBuffers 2

  GL.bindVertexArray vao1
  GL.bindBuffer GL.TargetArray vbo1
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  GL.bindVertexArray vao2
  GL.bindBuffer GL.TargetArray vbo2
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.7,  0.7, 0.0
    ,  0.8,  0.7, 0.0
    ,  0.8,  0.6, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- GL.stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao1
        GL.drawArrays GL.TypeTriangles 0 3
        GL.bindVertexArray vao2
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao1, vao2]
        GL.deleteBuffers [vbo1, vbo2]
        GL.deleteProgram prog

  return (render, cleanup)


doubleTrianglesDifferentColors :: IO (IO (), IO ())
doubleTrianglesDifferentColors = do
  [vao1, vao2] <- GL.genVertexArrays 2
  [vbo1, vbo2] <- GL.genBuffers 2

  GL.bindVertexArray vao1
  GL.bindBuffer GL.TargetArray vbo1
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  GL.bindVertexArray vao2
  GL.bindBuffer GL.TargetArray vbo2
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.7,  0.7, 0.0
    ,  0.8,  0.7, 0.0
    ,  0.8,  0.6, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog1 <- GL.stdProgram vertexShaderSrc fragmentShaderSrc
  prog2 <- GL.stdProgram vertexShaderSrc redFragmentShaderSrc

  let render = do
        GL.useProgram prog1
        GL.bindVertexArray vao1
        GL.drawArrays GL.TypeTriangles 0 3

        GL.useProgram prog2
        GL.bindVertexArray vao2
        GL.drawArrays GL.TypeTriangles 0 3

        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao1, vao2]
        GL.deleteBuffers [vbo1, vbo2]
        GL.deleteProgram prog1
        GL.deleteProgram prog2

  return (render, cleanup)



rectangleUsingElements :: IO (IO (), IO ())
rectangleUsingElements = do
  [vbo, ebo] <- GL.genBuffers 2
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5,  0
    ,  0.5, -0.5,  0
    , -0.5, -0.5,  0
    , -0.5,  0.5,  0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0

  GL.bindBuffer GL.TargetElementArray ebo
  GL.uintBufferData GL.TargetElementArray GL.UsageStaticDraw
    [ 0, 1, 2
    , 0, 2, 3
    ]

  GL.unbindVertexArray

  prog <- GL.stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao
        GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo, ebo]
        GL.deleteProgram prog
        return ()

  return (render, cleanup)

fragmentShaderSrc :: B.ByteString
fragmentShaderSrc = [r|#version 330 core
out vec4 color;

void main()
{
    color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}
|]

redFragmentShaderSrc :: B.ByteString
redFragmentShaderSrc = [r|#version 330 core
out vec4 color;

void main()
{
    color = vec4(1.0f, 0.0f, 0.0f, 1.0f);
}
|]


vertexShaderSrc :: B.ByteString
vertexShaderSrc = [r|#version 330 core
layout (location = 0) in vec3 position;

void main()
{
  gl_Position = vec4(position.x, position.y, position.z, 1.0);
}
|]
