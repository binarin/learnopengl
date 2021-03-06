{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Coordinate(staticMatrixStack) where

import Text.RawString.QQ
import Data.Default

import qualified GLWrap as GL
import qualified Data.Set as Set
import Linear.V3
import Linear.Matrix
import Control.Monad
import Control.Lens
import Data.Default
import Control.Monad.State (execState, get)
import Debug.Trace as Trace

import qualified Graphics.UI.GLFW as GLFW
import Behaviour
import Camera
import Utils

staticMatrixStack = mkBehaviour initialize frame render cleanup

data State = State { vbo :: GL.Buffer
                   , vao :: GL.VertexArray
                   , texCont :: GL.Texture
                   , texFace :: GL.Texture
                   , prog :: GL.Program
                   , _currentTime :: Float
                   , _frameTime :: Float
                   , _keys :: Keys
                   , _camera :: Camera
                   , _speed :: Float
                   , _sensitivity :: Float
                   , _fov :: Float
                   , _fovSensitivity :: Float
                   }


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

  let _currentTime = 0
  let _frameTime = 0
  let _keys = mempty
  let _camera = def & camPos .~ V3 0 0 3
                    & camUp .~ V3 0 1 0
                    & camYaw .~ (-pi / 2)

  let _sensitivity = 0.05
  let _speed = 0.5
  let _fov = 45
  let _fovSensitivity = 15
  return $ State {..}

appKeys :: Lens' State Keys
appKeys = lens _keys $ \st keys -> st { _keys = keys }

appTime :: Lens' State Float
appTime = lens _currentTime $ \st ct -> st { _currentTime = ct
                                           , _frameTime = ct - _currentTime st
                                           }

appDelta :: Lens' State Float
appDelta = lens _frameTime $ \st ft -> st { _frameTime = ft }

appCamera :: Lens' State Camera
appCamera = lens _camera $ \st c -> st { _camera = c }

appSpeed :: Lens' State Float
appSpeed = lens _speed $ \st s -> st { _speed = s }

appSensitivity :: Lens' State Float
appSensitivity = lens _sensitivity $ \st s -> st { _sensitivity = s }

appFov :: Lens' State Float
appFov = lens _fov $ \st f -> st { _fov = clamp 30 170 f }

appFovSensitivity :: Lens' State Float
appFovSensitivity = lens _fovSensitivity $ \st fs -> st { _fovSensitivity = fs }

frame :: [InputEvent] -> Float -> State -> State
frame events time st = flip execState st $ do
  appKeys %= processKeyEvents events
  appTime .= time
  keys <- use appKeys
  delta <- use appDelta
  appCamera %= processCameraCommands keys delta
  appCamera %= processCameraPan delta
  appFov %= (+ fovDelta * delta * st^.appFovSensitivity * (-1))
  return ()
  where
    processKeyEvents events oldKeys = foldl trackKeys oldKeys events

    panEvents = [ (x, y) | MouseEvent x y <- events ]
    (panYaw, panPitch) = foldl (\(ax, ay) (x, y) -> (ax + x, ay + y)) (0, 0) panEvents
    processCameraPan delta cam = cam & camYaw %~ (+ panYaw * delta * st^.appSensitivity)
                                     & camPitch %~ (+ panPitch * delta * st^.appSensitivity * (-1))

    fovEvents = [ y | ScrollEvent _ y <- events ]
    fovDelta = foldl (+) 0 fovEvents

    advance keys delta cam = case (Set.member GLFW.Key'W keys, Set.member GLFW.Key'S keys) of
      (True, _) -> camAdvance (delta * st^.appSpeed) cam
      (_, True) -> camAdvance (negate . (*delta) $ st^.appSpeed) cam
      (_, _) -> cam

    strafe keys delta cam = case (Set.member GLFW.Key'A keys, Set.member GLFW.Key'D keys) of
      (True, _) -> camStrafe (negate . (*delta) $ st^.appSpeed) cam
      (_, True) -> camStrafe (delta * st^.appSpeed) cam
      (_, _) -> cam

    processCameraCommands :: Keys -> Float -> Camera -> Camera
    processCameraCommands keys delta cam =
      foldl (\cam f -> f keys delta cam) cam [advance, strafe]

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
render (st@State{..}) width height = do
  tex1loc <- GL.getUniformLocation prog "ourTexture1"
  tex2loc <- GL.getUniformLocation prog "ourTexture2"
  modelLoc <- GL.getUniformLocation prog "model"
  viewLoc <- GL.getUniformLocation prog "view"
  projectionLoc <- GL.getUniformLocation prog "projection"

  let projection = GL.perspectiveMatrix (GL.Deg $ st^.appFov) (screenRatio width height) 0.1 100

  GL.useProgram prog

  GL.uniform2DTexture texCont GL.Texture0 tex1loc
  GL.uniform2DTexture texFace GL.Texture1 tex2loc

  GL.uniformMatrix4fv viewLoc $ viewMatrix _camera
  GL.uniformMatrix4fv projectionLoc projection

  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor, GL.ClearDepth]

  GL.bindVertexArray vao

  forM_ (zip cubePositions [0..]) $ \(pos, idx) -> do
    let addRot = if idx `mod` 3 == 0 then 10 * (st^.appTime) else 0
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
