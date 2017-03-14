{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LightingReflex where

import Data.Monoid
import qualified Data.Text as T
import Text.RawString.QQ
import Control.Monad.Reader
import Reflex
import Data.Default
import Linear.V3
import Linear.V4
import Linear.Vector (scaled, (^-^))
import Linear.Matrix hiding (trace)
import Linear.Metric (dot, normalize)
import Control.Lens
import qualified Data.ByteString as B

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
                       , locMaterial :: MaterialUniform
                       , locLightPos :: GL.UniformLocation
                       }

data Material = Material { matAmbient :: V3 Float
                         , matDiffuse :: V3 Float
                         , matSpecular :: V3 Float
                         , matShininess :: Float
                         }

instance Default Material where
  def = Material { matAmbient = 0.1
                 , matDiffuse = 0.3
                 , matSpecular = 0.5
                 , matShininess = 32
                 }

data MaterialUniform = MaterialUniform { locAmbient :: GL.UniformLocation
                                       , locDiffuse :: GL.UniformLocation
                                       , locSpecular :: GL.UniformLocation
                                       , locShininess :: GL.UniformLocation
                                       }

initMaterialUniform :: GL.Program -> B.ByteString -> IO MaterialUniform
initMaterialUniform prog structName = do
  MaterialUniform
    <$> GL.getUniformLocation prog (structName <> ".ambient")
    <*> GL.getUniformLocation prog (structName <> ".diffuse")
    <*> GL.getUniformLocation prog (structName <> ".specular")
    <*> GL.getUniformLocation prog (structName <> ".shininess")

vec3Uniform :: GL.UniformLocation -> V3 Float -> IO ()
vec3Uniform loc (V3 x y z) = GL.uniform3f loc x y z

materialUniform :: MaterialUniform -> Material -> IO ()
materialUniform MaterialUniform{..} Material{..} = do
  vec3Uniform locAmbient matAmbient
  vec3Uniform locDiffuse matDiffuse
  vec3Uniform locSpecular matSpecular
  GL.uniform1f locShininess matShininess


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
                           , _material :: Material
                           }
makeLenses ''GameState


data LitCubeColors = Colors (V3 Float) Material
renderLitCube :: MatrixStack -> V3 Float -> LitCubeColors -> LitCube -> IO ()
renderLitCube MatrixStack{stackModel, stackView, stackProjection}
              lightPos
              (Colors lightColor material)
              LitCube{..} = do
  GL.useProgram prog
  GL.uniformMatrix4fv locView stackView
  GL.uniformMatrix4fv locModel stackModel
  GL.uniformMatrix4fv locProjection stackProjection

  materialUniform locMaterial material
  GL.uniform3f locLightColor (lightColor^._x) (lightColor^._y) (lightColor^._z)

  let lpV = stackView !* point lightPos
  GL.uniform3f locLightPos (lpV^._x) (lpV^._y) (lpV^._z)

  GL.bindVertexArray vao
  GL.drawArrays GL.TypeTriangles 0 36


addNormals :: [V3 (V3 Float)] -> [Float]
addNormals = concatMap unpackAndAddNormal
  where
    unpackAndAddNormal (V3 (a@(V3 ax ay az)) (b@(V3 bx by bz)) (c@(V3 cx cy cz))) =
      let (V3 nx ny nz) = normalize $ (b ^-^ a) `cross` (c ^-^ a)
      in [ax, ay, az, nx, ny, nz
         ,bx, by, bz, nx, ny, nz
         ,cx, cy, cz, nx, ny, nz
         ]

mkLitCube :: IO LitCube
mkLitCube = do
  vbo <- GL.genBuffer
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo

  let v1 :: V3 Float = V3 (-0.5) (-0.5) ( 0.5)
      v2 = V3 ( 0.5) (-0.5) ( 0.5)
      v3 = V3 ( 0.5) ( 0.5) ( 0.5)
      v4 = V3 (-0.5) ( 0.5) ( 0.5)
      v5 = V3 (-0.5) (-0.5) (-0.5)
      v6 = V3 ( 0.5) (-0.5) (-0.5)
      v7 = V3 ( 0.5) ( 0.5) (-0.5)
      v8 = V3 (-0.5) ( 0.5) (-0.5)

      triangles = [ V3 v1 v2 v3
                  , V3 v3 v4 v1
                  , V3 v7 v3 v2
                  , V3 v2 v6 v7
                  , V3 v5 v1 v8
                  , V3 v8 v1 v4
                  , V3 v3 v7 v4
                  , V3 v7 v8 v4
                  , V3 v1 v6 v2
                  , V3 v1 v5 v6
                  , V3 v8 v7 v6
                  , V3 v5 v8 v6
                  ]

  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw (addNormals triangles)

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 24 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 3 GL.AttribPointerFloat False 24 12
  GL.enableVertexAttribArray 1

  prog <- GL.stdProgram
    ([r|
      #version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec3 normal;
      uniform mat4 model;
      uniform mat4 view;
      uniform mat4 projection;
      out vec3 Normal;
      out vec3 FragPos;

      void main() {
        gl_Position = projection * view * model * vec4(position, 1.0f);
        FragPos = vec3(view * model * vec4(position, 1.0f));
        Normal = mat3(transpose(inverse(view * model))) * normal;
      }
      |])
    ([r|
      #version 330 core
      uniform vec3 lightColor;
      uniform vec3 lightPos;

      struct Material {
        vec3 ambient;
        vec3 diffuse;
        vec3 specular;
        float shininess;
      };
      uniform Material material;

      in vec3 Normal;
      in vec3 FragPos;
      out vec4 color;

      void main() {
        vec3 ambient = material.ambient * lightColor;

        vec3 norm = normalize(Normal);
        vec3 lightDir = normalize(lightPos - FragPos);
        float diff = max(dot(norm, lightDir), 0.0);
        vec3 diffuse = (diff * material.diffuse) * lightColor;

        vec3 viewDir = normalize(-FragPos);
        vec3 reflectDir = reflect(-lightDir, norm);
        float spec = pow(max(dot(viewDir, reflectDir),0.0f), material.shininess);
        vec3 specular = (material.specular * spec) * lightColor;

        vec3 result = ambient + diffuse + specular;
        color = vec4(result, 1.0f);
      }
      |])
  locView <- GL.getUniformLocation prog "view"
  locModel <- GL.getUniformLocation prog "model"
  locProjection <- GL.getUniformLocation prog "projection"
  locMaterial <- initMaterialUniform prog "material"
  locLightColor <- GL.getUniformLocation prog "lightColor"
  locLightPos <- GL.getUniformLocation prog "lightPos"

  return $ LitCube {vao, vbo, prog, locProjection, locModel, locView, locMaterial, locLightColor, locLightPos}


renderer :: Renderer RenderState GameState
renderer = Renderer initR renderR cleanupR

initR _ = do
  ls <- mkWhiteCube
  litCube <- mkLitCube
  GL.enable GL.DepthTest
  return $ RenderState { _whiteCube = ls, _litCube = litCube }

renderR gs rs = do
  GL.clearColor $ GL.RGBA 0 0 0 1
  GL.clear [GL.ClearColor, GL.ClearDepth]

  let viewMat = povViewMat $ gs^.stPov
  let projectionMat = povProjectionMat (gs^.width) (gs^.height) (gs^.stPov)
  let lightModelMat = (GL.translationMatrix $ gs^.lightPos) !*! (scaled $ V4 0.2 0.2 0.2 1)

  renderWhiteCube (MatrixStack lightModelMat viewMat projectionMat) (rs^.whiteCube)

  let cubeModelMat = identity
  renderLitCube (MatrixStack cubeModelMat viewMat projectionMat) (gs^.lightPos) (Colors (gs^.lightColor) (gs^.material) ) (rs^.litCube)

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
  totalTime <- asks eventTime
  ti <- holdDyn 0.01 te
  cam <- camDyn CameraConfig
  let lightColor = constDyn $ V3 1 1 1
      material = constDyn $ def

  lightPos <- lift $ foldDyn (\a (V3 x y z) -> V3 x (sin a) z) (V3 1.2 1 2) totalTime
  let st = GameState <$> cam <*> lightPos <*> ti <*> constDyn (GL.Width 800) <*> constDyn (GL.Height 600) <*> lightColor <*> material
  return (renderer, current st)

go :: IO ()
go = host guest
