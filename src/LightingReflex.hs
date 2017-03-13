{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LightingReflex where

import Text.RawString.QQ
import Control.Monad.Reader
import Reflex
import Data.Default
import Linear.V3
import Linear.V4
import Linear.Vector (scaled)
import Linear.Matrix hiding (trace)
import Control.Lens

import GLHost
import qualified Data.Map as M
import qualified GLWrap as GL
import qualified Graphics.UI.GLFW as GLFW
import POV
import Camera
import Lighting (WhiteCube, mkWhiteCube, renderWhiteCube, MatrixStack(..))

data LitCube = LitCube { vao :: GL.VertexArray
                       , vbo :: GL.Buffer
                       , prog :: GL.Program
                       , locView :: GL.UniformLocation
                       , locModel :: GL.UniformLocation
                       , locProjection :: GL.UniformLocation
                       , locLightColor :: GL.UniformLocation
                       , locObjectColor :: GL.UniformLocation
                       }

data RenderState = RenderState { _whiteCube :: WhiteCube
                               , _litCube :: LitCube
                               }
makeLenses ''RenderState

data GameState = GameState { _stPov :: POV
                           , _lightPos :: V3 Float
                           , _delta :: Float
                           , _width :: GL.Width
                           , _height :: GL.Height
                           , _lightColor :: V3 Float
                           , _objectColor :: V3 Float
                           }
makeLenses ''GameState


data LitCubeColors = Colors (V3 Float) (V3 Float)
renderLitCube :: MatrixStack -> LitCubeColors -> LitCube -> IO ()
renderLitCube MatrixStack{stackModel, stackView, stackProjection}
              (Colors lightColor objectColor)
              LitCube{..} = do
  GL.useProgram prog
  GL.uniformMatrix4fv locView stackView
  GL.uniformMatrix4fv locModel stackModel
  GL.uniformMatrix4fv locProjection stackProjection
  GL.uniform3f locObjectColor (objectColor^._x) (objectColor^._y) (objectColor^._z)
  GL.uniform3f locLightColor (lightColor^._x) (lightColor^._y) (lightColor^._z)

  GL.bindVertexArray vao
  GL.drawArrays GL.TypeTriangles 0 36




mkLitCube :: IO LitCube
mkLitCube = do
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
      uniform vec3 objectColor;
      uniform vec3 lightColor;
      out vec4 color;
      void main() {
        color = vec4(lightColor * objectColor, 1.0f);
      }
      |])
  locView <- GL.getUniformLocation prog "view"
  locModel <- GL.getUniformLocation prog "model"
  locProjection <- GL.getUniformLocation prog "projection"
  locObjectColor <- GL.getUniformLocation prog "objectColor"
  locLightColor <- GL.getUniformLocation prog "lightColor"

  return $ LitCube {vao, vbo, prog, locProjection, locModel, locView, locObjectColor, locLightColor}


renderer :: Renderer RenderState GameState
renderer = Renderer initR renderR cleanupR

initR _ = do
  ls <- mkWhiteCube
  litCube <- mkLitCube
  return $ RenderState { _whiteCube = ls, _litCube = litCube }

renderR gs rs = do
  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor, GL.ClearDepth]

  let viewMat = povViewMat $ gs^.stPov
  let projectionMat = povProjectionMat (gs^.width) (gs^.height) (gs^.stPov)
  let lightModelMat = (GL.translationMatrix $ gs^.lightPos) !*! (scaled $ V4 0.2 0.2 0.2 1)

  renderWhiteCube (MatrixStack lightModelMat viewMat projectionMat) (rs^.whiteCube)

  let cubeModelMat = identity
  renderLitCube (MatrixStack cubeModelMat viewMat projectionMat) (Colors (gs^.lightColor)  (gs^.objectColor)) (rs^.litCube)

  return rs

cleanupR _ = do
  return ()

data CameraConfig = CameraConfig

tickEvent :: GLMonad t m (Dynamic t Float)
tickEvent = do
  et <- asks eventTime
  delta <- fmap snd <$> foldDyn (\abs (prev, delta) -> (abs, abs -prev)) (0, 0) et
  return delta

keyToggle :: a -> [(GLFW.Key, a)] -> GLMonad t m (Dynamic t a)
keyToggle noKeyValue bindings = do
  onEvents <- mapM (\(key, val) -> do
                       ev <- keyEvent key
                       return $ const val <$> ffilter id ev
                   ) bindings
  allKeyEvents <- mapM (keyEvent . fst) bindings
  let offEvent = const noKeyValue <$> ffilter not (leftmost allKeyEvents)
      targetEvent = leftmost $ onEvents ++ [offEvent]
  lift $ holdDyn noKeyValue targetEvent


camDyn :: forall t m. CameraConfig -> GLMonad t m (Dynamic t POV)
camDyn conf = do
  tick <- asks deltaTickEvent
  move <- keyToggle Nothing [(GLFW.Key'W, Just MoveForward)
                            ,(GLFW.Key'S, Just MoveBackward)
                            ]
  strafe <- keyToggle Nothing [(GLFW.Key'A, Just StrafeLeft)
                              ,(GLFW.Key'D, Just StrafeRight)
                              ]
  cursor <- asks cursorDyn
  scroll <- asks scrollEvent
  (ix, iy) <- lift $ sample $ current cursor
  cursorDeltaDyn <- fmap fst <$> foldDyn (\(cx, cy) ((dx, dy), (px, py)) -> ((cx - px, cy - py), (cx, cy))) ((0, 0), (ix, iy)) (updated cursor)
  let cursorDeltaForTick = leftmost [Just <$> updated cursorDeltaDyn, const Nothing <$> tick]
  pan <- lift $ holdDyn Nothing cursorDeltaForTick
  fov <- lift $ holdDyn Nothing $ leftmost [Just . snd <$> scroll, const Nothing <$> tick]
  td <- holdDyn 0.01 tick
  let a :: Dynamic t Advance = Advance <$> move <*> strafe <*> pan <*> fov
      ea = (,) <$> a <*> td

  lift $ foldDyn (\(adv, dt) pov -> advancePov adv dt pov) def (updated ea)

guest :: GLApp t m RenderState GameState
guest = do
  te <- asks deltaTickEvent
  ti <- holdDyn 0.01 te
  cam <- camDyn CameraConfig
  let lightPos = constDyn $ V3 1.2 1 2
      lightColor = constDyn $ V3 1 1 1
      objectColor = constDyn $ V3 1 0.5 0.3

  let st = GameState <$> cam <*> lightPos <*> ti <*> constDyn (GL.Width 800) <*> constDyn (GL.Height 600) <*> lightColor <*> objectColor
  return (renderer, current st)

go :: IO ()
go = host guest
