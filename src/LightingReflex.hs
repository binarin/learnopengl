{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Monoid
import qualified Data.Text as T
import Text.RawString.QQ
import Control.Monad.Reader
import Reflex
import Data.Default
import Linear.V3
import Linear.V2
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
                       , locMaterial :: MaterialUniform
                       , locLight :: LightUniform
                       }

data Light = Light { _lightPosition :: V3 Float
                   , _lightAmbient :: V3 Float
                   , _lightDiffuse :: V3 Float
                   , _lightSpecular :: V3 Float
                   }

data LightUniform = LightUniform { locPosition :: GL.UniformLocation
                                 , locAmbient :: GL.UniformLocation
                                 , locDiffuse :: GL.UniformLocation
                                 , locSpecular :: GL.UniformLocation
                                 }

data Material = Material { _matDiffuseImage :: FilePath
                         , _matSpecularImage :: FilePath
                         , _matShininess :: Float
                         }

data MaterialUniform = MaterialUniform { locShininess :: GL.UniformLocation
                                       , locTexture :: GL.UniformLocation
                                       , locSpecularTexture :: GL.UniformLocation
                                       , textureId :: GL.Texture
                                       , specularTextureId :: GL.Texture
                                       }

data RenderState = RenderState { _whiteCube :: WhiteCube
                               , _litCube :: LitCube
                               }

data GameState = GameState { _stPov :: POV
                           , _delta :: Float
                           , _width :: GL.Width
                           , _height :: GL.Height
                           , _material :: Material
                           , _light :: Light
                           }


data LitCubeColors = Colors (V3 Float) Material

makeLenses ''Material
makeLenses ''Light
makeLenses ''RenderState
makeLenses ''GameState


initMaterialUniform :: Material -> GL.Program -> B.ByteString -> IO MaterialUniform
initMaterialUniform mat prog structName = do
  [textureId, specularTextureId] <- GL.genTextures 2

  GL.bindTexture GL.Texture2D textureId
  GL.texParameter GL.Texture2D (GL.TextureWrapS GL.ClampToEdge)
  GL.texParameter GL.Texture2D (GL.TextureWrapT GL.ClampToEdge)
  GL.texParameter GL.Texture2D (GL.TextureMinFilter GL.MinLinear)
  GL.texParameter GL.Texture2D (GL.TextureMagFilter GL.MagLinear)
  GL.texImage2D $ mat^.matDiffuseImage
  GL.generateMipmap GL.Texture2D
  GL.unbindTexture GL.Texture2D

  GL.bindTexture GL.Texture2D specularTextureId
  GL.texParameter GL.Texture2D (GL.TextureWrapS GL.ClampToEdge)
  GL.texParameter GL.Texture2D (GL.TextureWrapT GL.ClampToEdge)
  GL.texParameter GL.Texture2D (GL.TextureMinFilter GL.MinLinear)
  GL.texParameter GL.Texture2D (GL.TextureMagFilter GL.MagLinear)
  GL.texImage2D $ mat^.matSpecularImage
  GL.generateMipmap GL.Texture2D
  GL.unbindTexture GL.Texture2D

  MaterialUniform
    <$> GL.getUniformLocation prog (structName <> ".shininess")
    <*> GL.getUniformLocation prog (structName <> ".diffuse")
    <*> GL.getUniformLocation prog (structName <> ".specular")
    <*> pure textureId
    <*> pure specularTextureId

vec3Uniform :: GL.UniformLocation -> V3 Float -> IO ()
vec3Uniform loc (V3 x y z) = GL.uniform3f loc x y z

materialUniform :: MaterialUniform -> Material -> IO ()
materialUniform MaterialUniform{..} Material{..} = do
  GL.uniform1f locShininess _matShininess

  GL.activeTexture GL.Texture0
  GL.bindTexture GL.Texture2D textureId
  GL.uniform1i locTexture 0

  GL.activeTexture GL.Texture1
  GL.bindTexture GL.Texture2D specularTextureId
  GL.uniform1i locSpecularTexture 1

renderLitCube :: MatrixStack -> Material -> Light -> LitCube -> IO ()
renderLitCube MatrixStack{stackModel, stackView, stackProjection}
              material
              light
              LitCube{..} = do
  GL.useProgram prog
  GL.uniformMatrix4fv locView stackView
  GL.uniformMatrix4fv locModel stackModel
  GL.uniformMatrix4fv locProjection stackProjection

  materialUniform locMaterial material
  lightUniform locLight light stackView

  GL.bindVertexArray vao
  GL.drawArrays GL.TypeTriangles 0 36


type VecWithTexCoords = (V3 Float, V2 Float)
addNormals :: [(VecWithTexCoords, VecWithTexCoords, VecWithTexCoords)] -> [Float]
addNormals = concatMap unpackAndAddNormal
  where
    unpackAndAddNormal ((a@(V3 ax ay az), V2 as at), (b@(V3 bx by bz), V2 bs bt), (c@(V3 cx cy cz), V2 cs ct)) =
      let (V3 nx ny nz) = normalize $ (b ^-^ a) `cross` (c ^-^ a)
      in [ax, ay, az, nx, ny, nz, as, at
         ,bx, by, bz, nx, ny, nz, bs, bt
         ,cx, cy, cz, nx, ny, nz, cs, ct
         ]

mkLitCube :: Material -> IO LitCube
mkLitCube mat = do
  vbo <- GL.genBuffer
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo

  let v1 = V3 (-0.5) (-0.5) ( 0.5)
      v2 = V3 ( 0.5) (-0.5) ( 0.5)
      v3 = V3 ( 0.5) ( 0.5) ( 0.5)
      v4 = V3 (-0.5) ( 0.5) ( 0.5)
      v5 = V3 (-0.5) (-0.5) (-0.5)
      v6 = V3 ( 0.5) (-0.5) (-0.5)
      v7 = V3 ( 0.5) ( 0.5) (-0.5)
      v8 = V3 (-0.5) ( 0.5) (-0.5)
      add2 a b v = (v, V2 a b)
      tl = add2 0 0
      tr = add2 1 0
      bl = add2 0 1
      br = add2 1 1

      triangles = [ (bl v1, br v2, tr v3)
                  , (tr v3, tl v4, bl v1)

                  , (tr v7, tl v3, bl v2)
                  , (bl v2, br v6, tr v7)

                  , (bl v5, br v1, tl v8)
                  , (tl v8, br v1, tr v4)

                  , (br v3, tr v7, bl v4)
                  , (tr v7, tl v8, bl v4)

                  , (tl v1, br v6, tr v2)
                  , (tl v1, bl v5, br v6)

                  , (bl v6, br v5, tr v8)
                  , (tr v8, tl v7, bl v6)
                  ]

  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw (addNormals triangles)

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 32 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 3 GL.AttribPointerFloat False 32 12
  GL.enableVertexAttribArray 1

  GL.vertexAttribPointer 2 3 GL.AttribPointerFloat False 32 24
  GL.enableVertexAttribArray 2

  prog <- GL.stdProgram
    ([r|
      #version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec3 normal;
      layout (location = 2) in vec2 texCoords;

      uniform mat4 model;
      uniform mat4 view;
      uniform mat4 projection;
      out vec3 Normal;
      out vec3 FragPos;
      out vec2 TexCoords;

      void main() {
        gl_Position = projection * view * model * vec4(position, 1.0f);
        FragPos = vec3(view * model * vec4(position, 1.0f));
        Normal = mat3(transpose(inverse(view * model))) * normal;
        TexCoords = texCoords;
      }
      |])
    ([r|
      #version 330 core
      struct Material {
        sampler2D diffuse;
        sampler2D specular;
        float shininess;
      };
      uniform Material material;

      struct Light {
        vec3 position;
        // colors
        vec3 ambient;
        vec3 diffuse;
        vec3 specular;
      };
      uniform Light light;

      in vec3 Normal;
      in vec3 FragPos;
      in vec2 TexCoords;
      out vec4 color;

      void main() {

        vec3 norm = normalize(Normal);
        vec3 lightDir = normalize(light.position - FragPos);
        float diff = max(dot(norm, lightDir), 0.0);

        vec3 viewDir = normalize(-FragPos);
        vec3 reflectDir = reflect(-lightDir, norm);
        float spec = pow(max(dot(viewDir, reflectDir),0.0f), material.shininess);

        vec3 ambient = light.ambient * vec3(texture(material.diffuse, TexCoords));
        vec3 diffuse = light.diffuse * diff * vec3(texture(material.diffuse, TexCoords));
        vec3 specular = light.specular * spec * vec3(texture(material.specular, TexCoords));


        color = vec4(ambient + diffuse + specular, 1.0f);
      }
      |])
  locView <- GL.getUniformLocation prog "view"
  locModel <- GL.getUniformLocation prog "model"
  locProjection <- GL.getUniformLocation prog "projection"
  locMaterial <- initMaterialUniform mat prog "material"
  locLight <- initLightUniform prog "light"

  return $ LitCube {vao, vbo, prog, locProjection, locModel, locView, locMaterial, locLight}

renderer :: Renderer RenderState GameState
renderer = Renderer initR renderR cleanupR

initR gs = do
  ls <- mkWhiteCube
  litCube <- mkLitCube $ gs^.material
  GL.enable GL.DepthTest
  return $ RenderState { _whiteCube = ls, _litCube = litCube }

renderR gs rs = do
  GL.clearColor $ GL.RGBA 0 0 0 1
  GL.clear [GL.ClearColor, GL.ClearDepth]

  let viewMat = povViewMat $ gs^.stPov
  let projectionMat = povProjectionMat (gs^.width) (gs^.height) (gs^.stPov)
  let lightModelMat = (GL.translationMatrix $ gs^.light.lightPosition) !*! (scaled $ V4 0.2 0.2 0.2 1)

  renderWhiteCube (MatrixStack lightModelMat viewMat projectionMat) (rs^.whiteCube)

  let cubeModelMat = identity
  renderLitCube (MatrixStack cubeModelMat viewMat projectionMat) (gs^.material) (gs^.light) (rs^.litCube)

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
  vertMove <- keyToggle Nothing [(GLFW.Key'Space, Just LiftUp)
                                ,(GLFW.Key'C, Just LiftDown)
                                ]
  cursor <- asks cursorDyn
  scroll <- asks scrollEvent
  (ix, iy) <- lift $ sample $ current cursor
  cursorDeltaDyn <- fmap fst <$> foldDyn (\(cx, cy) ((dx, dy), (px, py)) -> ((cx - px, cy - py), (cx, cy))) ((0, 0), (ix, iy)) (updated cursor)
  let cursorDeltaForTick = leftmost [Just <$> updated cursorDeltaDyn, const Nothing <$> tick]
  pan <- lift $ holdDyn Nothing cursorDeltaForTick
  fov <- lift $ holdDyn Nothing $ leftmost [Just . snd <$> scroll, const Nothing <$> tick]
  td <- holdDyn 0.01 tick
  let a :: Dynamic t Advance = Advance <$> move <*> strafe <*> vertMove <*> pan <*> fov
      ea = (,) <$> a <*> td

  lift $ foldDyn (\(adv, dt) pov -> advancePov adv dt pov) def (updated ea)

guest :: GLApp t m RenderState GameState
guest = do
  te <- asks deltaTickEvent
  totalTime <- asks eventTime
  ti <- holdDyn 0.01 te
  cam <- camDyn CameraConfig
  let material = constDyn $ def

  lightPos <- lift $ foldDyn (\a (V3 x y z) -> V3 x (sin a) z) (V3 1.2 1 2) totalTime
  let lightColor = fmap (\t -> V3 (2 * sin t) (0.7 * sin t) (1.3 * sin t)) totalTime
      diffuseColor = (*0.5) <$> lightColor
      ambientColor = (*0.2) <$> lightColor

  lightColorDyn <- lift $ holdDyn 0 lightColor
  diffuseColorDyn <- lift $ holdDyn 0 diffuseColor
  ambientColorDyn <- lift $ holdDyn 0 ambientColor
  let
    light = Light <$> constDyn (V3 1.2 1 2) <*> constDyn 1 <*> constDyn 1 <*> constDyn 1
    st = GameState <$> cam <*> ti <*> constDyn (GL.Width 800) <*> constDyn (GL.Height 600) <*> material <*> light

  return (renderer, current st)

main :: IO ()
main = host guest

instance Default Light where
  def = Light { _lightPosition = V3 1.2 1 2
              , _lightAmbient = 0.2
              , _lightDiffuse = 0.5
              , _lightSpecular = 1
              }

initLightUniform :: GL.Program -> B.ByteString -> IO LightUniform
initLightUniform prog prefix = do
  LightUniform
    <$> GL.getUniformLocation prog (prefix <> ".position")
    <*> GL.getUniformLocation prog (prefix <> ".ambient")
    <*> GL.getUniformLocation prog (prefix <> ".diffuse")
    <*> GL.getUniformLocation prog (prefix <> ".specular")

lightUniform :: LightUniform -> Light -> M44 Float -> IO ()
lightUniform LightUniform{..} Light{..} viewMat = do
  vec3Uniform locPosition _lightPosition
  vec3Uniform locAmbient _lightAmbient
  vec3Uniform locDiffuse _lightDiffuse
  vec3Uniform locSpecular _lightSpecular

  let lpV = viewMat !* point _lightPosition
  GL.uniform3f locPosition (lpV^._x) (lpV^._y) (lpV^._z)


instance Default Material where
  def = Material { _matDiffuseImage = "container2.png"
                 , _matSpecularImage = "container2_specular.png"
                 , _matShininess = 32
                 }
