{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Lighting where

import qualified GLWrap as GL
import Linear.Matrix hiding (trace)
import Linear.V4
import Linear.V3
import Text.RawString.QQ
import Control.Lens
import Debug.Trace (trace)

import POV
import Behaviour
import Data.Default
import Camera

data MatrixStack = MatrixStack { stackModel :: M44 Float
                               , stackView :: M44 Float
                               , stackProjection :: M44 Float
                               }

data WhiteCube = WhiteCube { vao :: GL.VertexArray
                           , vbo :: GL.Buffer
                           , prog :: GL.Program
                           , locView :: GL.UniformLocation
                           , locModel :: GL.UniformLocation
                           , locProjection :: GL.UniformLocation
                           }

data State = State { _stPov :: POV
                   , _lightMarker :: WhiteCube
                   , _stateKeys :: Keys
                   }

makeLenses ''State

lightingDemo = mkBehaviour initialize frame render cleanup

initialize :: IO (State)
initialize = do
  let _stPov = def & povCamera . camPos .~ (V3 0 0 10)
                   & povCamera . camYaw .~ (-pi/2)
  _lightMarker <- mkWhiteCube
  let _stateKeys = mempty
  return $ State{..}


frame :: [InputEvent] -> Float -> State -> State
frame events delta st = st


render :: State -> GL.Width -> GL.Height -> IO ()
render st width height = do
  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor, GL.ClearDepth]

  let viewMat = povViewMat $ st^.stPov
  let projectionMat = povProjectionMat width height (st^.stPov)
  let lightModelMat = identity
  renderWhiteCube (MatrixStack lightModelMat viewMat projectionMat) (st^.lightMarker)

cleanup :: State -> IO ()
cleanup st = do
  return ()

mkWhiteCube :: IO WhiteCube
mkWhiteCube = do
  vbo <- GL.genBuffer
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo

  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
   [ -0.5, -0.5, -0.5,
      0.5, -0.5, -0.5,
      0.5,  0.5, -0.5,
      0.5,  0.5, -0.5,
     -0.5,  0.5, -0.5,
     -0.5, -0.5, -0.5,

     -0.5, -0.5,  0.5,
      0.5, -0.5,  0.5,
      0.5,  0.5,  0.5,
      0.5,  0.5,  0.5,
     -0.5,  0.5,  0.5,
     -0.5, -0.5,  0.5,

     -0.5,  0.5,  0.5,
     -0.5,  0.5, -0.5,
     -0.5, -0.5, -0.5,
     -0.5, -0.5, -0.5,
     -0.5, -0.5,  0.5,
     -0.5,  0.5,  0.5,

      0.5,  0.5,  0.5,
      0.5,  0.5, -0.5,
      0.5, -0.5, -0.5,
      0.5, -0.5, -0.5,
      0.5, -0.5,  0.5,
      0.5,  0.5,  0.5,

     -0.5, -0.5, -0.5,
      0.5, -0.5, -0.5,
      0.5, -0.5,  0.5,
      0.5, -0.5,  0.5,
     -0.5, -0.5,  0.5,
     -0.5, -0.5, -0.5,

     -0.5,  0.5, -0.5,
      0.5,  0.5, -0.5,
      0.5,  0.5,  0.5,
      0.5,  0.5,  0.5,
     -0.5,  0.5,  0.5,
     -0.5,  0.5, -0.5
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0

  prog <- GL.stdProgram
    ([r|
      #version 330 core
      layout (location = 0) in vec3 position;
      uniform mat4 model;
      uniform mat4 view;
      uniform mat4 projection;
      void main() {
        gl_Position = projection * view * model * vec4(position, 1.0f);
      }
      |])
    ([r|
      #version 330 core
      out vec4 color;
      void main() {
        color = vec4(1.0f);
      }
      |])

  locView <- GL.getUniformLocation prog "view"
  locModel <- GL.getUniformLocation prog "model"
  locProjection <- GL.getUniformLocation prog "projection"

  return $ WhiteCube { vao, vbo, prog, locProjection, locModel, locView }

renderWhiteCube :: MatrixStack -> WhiteCube -> IO ()
renderWhiteCube MatrixStack{stackModel, stackView, stackProjection} WhiteCube{vao, prog, locView, locModel, locProjection} = do
  GL.useProgram prog
  GL.uniformMatrix4fv locView stackView
  GL.uniformMatrix4fv locModel stackModel
  GL.uniformMatrix4fv locProjection stackProjection

  GL.bindVertexArray vao
  GL.drawArrays GL.TypeTriangles 0 36

  return ()
