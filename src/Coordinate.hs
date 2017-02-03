{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Coordinate(staticMatrixStack) where

import Text.RawString.QQ
import Data.Default

import qualified GLWrap as GL
import Linear.V3
import Linear.Matrix
import Control.Monad

import Behaviour

staticMatrixStack = mkBehaviour initialize frame render cleanup

data State = State { vbo :: GL.Buffer
                   , vao :: GL.VertexArray
                   , texCont :: GL.Texture
                   , texFace :: GL.Texture
                   , prog :: GL.Program
                   , model :: M44 Float
                   , view :: M44 Float
                   , currentTime :: Float
                   }


cubeWithTextureCoords :: IO (GL.VertexArray, GL.Buffer)
cubeWithTextureCoords = do
  vbo <- GL.genBuffer
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
   [ -0.5, -0.5, -0.5,  0.0, 0.0,
      0.5, -0.5, -0.5,  1.0, 0.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
     -0.5,  0.5, -0.5,  0.0, 1.0,
     -0.5, -0.5, -0.5,  0.0, 0.0,

     -0.5, -0.5,  0.5,  0.0, 0.0,
      0.5, -0.5,  0.5,  1.0, 0.0,
      0.5,  0.5,  0.5,  1.0, 1.0,
      0.5,  0.5,  0.5,  1.0, 1.0,
     -0.5,  0.5,  0.5,  0.0, 1.0,
     -0.5, -0.5,  0.5,  0.0, 0.0,

     -0.5,  0.5,  0.5,  1.0, 0.0,
     -0.5,  0.5, -0.5,  1.0, 1.0,
     -0.5, -0.5, -0.5,  0.0, 1.0,
     -0.5, -0.5, -0.5,  0.0, 1.0,
     -0.5, -0.5,  0.5,  0.0, 0.0,
     -0.5,  0.5,  0.5,  1.0, 0.0,

      0.5,  0.5,  0.5,  1.0, 0.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
      0.5, -0.5, -0.5,  0.0, 1.0,
      0.5, -0.5, -0.5,  0.0, 1.0,
      0.5, -0.5,  0.5,  0.0, 0.0,
      0.5,  0.5,  0.5,  1.0, 0.0,

     -0.5, -0.5, -0.5,  0.0, 1.0,
      0.5, -0.5, -0.5,  1.0, 1.0,
      0.5, -0.5,  0.5,  1.0, 0.0,
      0.5, -0.5,  0.5,  1.0, 0.0,
     -0.5, -0.5,  0.5,  0.0, 0.0,
     -0.5, -0.5, -0.5,  0.0, 1.0,

     -0.5,  0.5, -0.5,  0.0, 1.0,
      0.5,  0.5, -0.5,  1.0, 1.0,
      0.5,  0.5,  0.5,  1.0, 0.0,
      0.5,  0.5,  0.5,  1.0, 0.0,
     -0.5,  0.5,  0.5,  0.0, 0.0,
     -0.5,  0.5, -0.5,  0.0, 1.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 20 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 2 GL.AttribPointerFloat False 20 12
  GL.enableVertexAttribArray 1

  GL.enable GL.DepthTest

  return $ (vao, vbo)

quadWithTextureCoords :: IO (GL.VertexArray, GL.Buffer)
quadWithTextureCoords = do
  vbo <- GL.genBuffer
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5,  0,   1, 1
    ,  0.5, -0.5,  0,   1, 0
    , -0.5, -0.5,  0,   0, 0
    , -0.5, -0.5,  0,   0, 0
    , -0.5,  0.5,  0,   0, 1
    ,  0.5,  0.5,  0,   1, 1
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 20 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 2 GL.AttribPointerFloat False 20 12
  GL.enableVertexAttribArray 1

  return $ (vao, vbo)


initialize :: IO State
initialize = do
  (vao, vbo) <- cubeWithTextureCoords
  texCont <- GL.load2DTexture def "container.jpg"
  texFace <- GL.load2DTexture def "awesomeface.png"

  prog <- GL.stdProgram
    [r|#version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec2 texCoord;
      out vec2 TexCoord;
      uniform mat4 model;
      uniform mat4 view;
      uniform mat4 projection;

      void main() {
        gl_Position = projection * view * model * vec4(position, 1.0f);
        TexCoord = vec2(texCoord.x, 1.0f-texCoord.y);
      }
      |]
    [r|#version 330 core
      in vec2 TexCoord;
      out vec4 color;
      uniform sampler2D ourTexture1;
      uniform sampler2D ourTexture2;
      void main() {
        color = mix(texture(ourTexture1, TexCoord), texture(ourTexture2, TexCoord), 0.2);
      }
      |]

  let model = identity
  let view = identity
  let currentTime = 0
  return $ State {..}

frame :: [InputEvent] -> Float -> State -> State
frame _ time st =
  let model = GL.rotationMatrix (GL.Deg $ time * 50) (V3 0.5 1 0)
      view = GL.translationMatrix (V3 0 0 (-3))
  in
    st{ model = model
      , view = view
      , currentTime = time
      }

screenRatio :: GL.Width -> GL.Height -> Float
screenRatio (GL.Width w) (GL.Height h) = fromIntegral w / fromIntegral h


cubePositions :: [V3 Float]
cubePositions = [
     V3 0.0 0.0 0.0,
     V3 2.0 5.0 (-15.0),
     V3 (-1.5) (-2.2) (-2.5),
     V3 (-3.8) (-2.0) (-12.3),
     V3 2.4 (-0.4) (-3.5),
     V3 (-1.7)  3.0 (-7.5),
     V3 1.3 (-2.0) (-2.5),
     V3 1.5  2.0 (-2.5),
     V3 1.5  0.2 (-1.5),
     V3 (-1.3)  1.0 (-1.5)
    ]

render :: State -> GL.Width -> GL.Height -> IO ()
render State{..} width height = do
  tex1loc <- GL.getUniformLocation prog "ourTexture1"
  tex2loc <- GL.getUniformLocation prog "ourTexture2"
  modelLoc <- GL.getUniformLocation prog "model"
  viewLoc <- GL.getUniformLocation prog "view"
  projectionLoc <- GL.getUniformLocation prog "projection"

  let projection = GL.perspectiveMatrix (GL.Deg 45) (screenRatio width height) 0.1 100

  GL.useProgram prog

  GL.uniform2DTexture texCont GL.Texture0 tex1loc
  GL.uniform2DTexture texFace GL.Texture1 tex2loc

  GL.uniformMatrix4fv viewLoc view
  GL.uniformMatrix4fv projectionLoc projection

  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor, GL.ClearDepth]

  GL.bindVertexArray vao

  forM_ (zip cubePositions [0..]) $ \(pos, idx) -> do
    let addRot = if idx `mod` 3 == 0 then 10 * currentTime else 0
    let model = GL.translationMatrix pos !*! GL.rotationMatrix (GL.Deg $ 20.0 * fromIntegral idx + addRot) (V3 1 0.3 0.5)
    GL.uniformMatrix4fv modelLoc model
    GL.drawArrays GL.TypeTriangles 0 36

  GL.unbindVertexArray


cleanup :: State -> IO ()
cleanup State{..} = do
  GL.disable GL.DepthTest
  GL.deleteBuffers [vbo]
  GL.deleteVertexArrays [vao]
  GL.deleteProgram prog
  return ()
