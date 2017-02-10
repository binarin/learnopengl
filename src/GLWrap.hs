{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module GLWrap ( createShader
              , ShaderError
              , createProgram
              , ProgramError
              , LL.Shader
              , LL.ShaderType(..)
              , LL.Program
              , LL.useProgram
              , LL.deleteShader
              , LL.AttribPointerType(..)
              , LL.vertexAttribPointer
              , LL.enableVertexAttribArray
              , LL.bindVertexArray
              , LL.VertexArray
              , LL.genVertexArray
              , LL.genVertexArrays
              , LL.unbindVertexArray
              , LL.drawArrays
              , LL.PrimitiveType(..)
              , LL.Buffer
              , LL.genBuffer
              , LL.genBuffers
              , LL.BufferTarget(..)
              , LL.bindBuffer
              , LL.BufferUsage(..)
              , LL.uintBufferData
              , LL.floatBufferData
              , LL.DrawElementType(..)
              , LL.drawElements
              , LL.MaterialFace(..)
              , LL.PolygonMode(..)
              , LL.polygonMode
              , LL.WinCoord(..)
              , LL.Width(..)
              , LL.Height(..)
              , LL.viewport
              , LL.toWinCoord
              , LL.toHeight
              , LL.toWidth
              , LL.RGBA(..)
              , LL.clearColor
              , LL.ClearBufferBit(..)
              , LL.ClearBufferMask
              , LL.clear
              , LL.deleteVertexArrays
              , LL.deleteBuffers
              , LL.deleteProgram
              , LL.UniformLocation
              , getUniformLocation
              , LL.uniform4f
              , LL.uniform1f
              , LL.uniform1i
              , LL.Texture
              , LL.genTexture
              , LL.genTextures
              , LL.bindTexture
              , LL.unbindTexture
              , LL.TextureTarget(..)
              , texImage2D
              , LL.texParameter
              , LL.TextureWrapParameter(..)
              , LL.TextureParameter(..)
              , LL.TextureMinFilter(..)
              , LL.TextureMagFilter(..)
              , LL.deleteTextures
              , LL.generateMipmap
              , LL.TextureUnit(..)
              , LL.activeTexture
              , stdProgram
              , LL.uniformMatrix4fv
              , Texture2DParams(..)
              , load2DTexture
              , Angle(..)
              , rotationMatrix
              , perspectiveMatrix
              , translationMatrix
              , uniform2DTexture
              , LL.Capability(..)
              , LL.enable
              , LL.disable
              ) where

import Graphics.GL.Core33
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek)
import Foreign.Ptr
import Data.ByteString as B
import qualified Data.Text as T
import Data.Typeable
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Text.Encoding (decodeUtf8With)
import Data.Default
import Linear.Matrix
import Linear.V3
import Linear.Quaternion
import Linear.Projection
import Control.Lens
import qualified Codec.Picture as P

import qualified GLWrap.LowLevel as LL
import GLWrap.LowLevel (Shader, ShaderType, Program, useProgram, deleteShader)

data ShaderError = ShaderError T.Text deriving (Typeable)
instance Exception ShaderError
instance Show ShaderError where
  show (ShaderError t) = "Shader compilation failed: " <> T.unpack t

data ProgramError = ProgramError T.Text deriving (Typeable)
instance Exception ProgramError
instance Show ProgramError where
  show (ProgramError t) = "Program compilation failed: " <> T.unpack t


createShader :: ShaderType -> ByteString -> IO Shader
createShader shaderType src = do
  shader <- LL.createShader shaderType
  LL.shaderSource shader src
  compileShader shader
  return shader

compileShader :: Shader -> IO ()
compileShader shader = do
  LL.compileShader shader
  status <- LL.getShader'compileStatus shader
  case status of
    True -> return ()
    False -> do
      err <- LL.getShaderInfoLog shader
      throw $ ShaderError $ decodeUtf8With (\_ _ -> Just '?') err

bs2Text :: B.ByteString -> T.Text
bs2Text = decodeUtf8With (\_ _ -> Just '?')

createProgram :: [Shader] -> IO Program
createProgram shaders = do
  prog <- LL.createProgram
  mapM_ (LL.attachShader prog) shaders
  LL.linkProgram prog
  status <- LL.getProgram'linkStatus prog
  case status of
    True -> return prog
    False -> do
      err <- LL.getProgramInfoLog prog
      throw $ ProgramError $ decodeUtf8With (\_ _ -> Just '?') err

getUniformLocation :: Program -> ByteString -> IO LL.UniformLocation
getUniformLocation prog name = do
  maybe (throw $ ProgramError $ "No uniform named " <> bs2Text name) id <$> LL.getUniformLocation prog name

data TextureError = TextureError T.Text deriving (Typeable)
instance Exception TextureError
instance Show TextureError where
  show (TextureError t) = "Texture error: " <> T.unpack t

either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' val left right = either left right val

texImage2D :: FilePath -> IO ()
texImage2D file = do
  imageResult <- P.readImage file
  either' imageResult (throw . TextureError . T.pack) $ \image -> do
    let P.Image{..} = P.convertRGB8 image
    LL.texImage2D LL.Texture2D 0 LL.InternalFormatRGB
      (LL.toWidth imageWidth) (LL.toHeight imageHeight) LL.PixelRGB LL.PixelGLubyte
      imageData

stdProgram :: B.ByteString -> B.ByteString -> IO LL.Program
stdProgram vertexSrc fragmentSrc = do
  vertexShader <- createShader LL.VertexShader vertexSrc
  fragmentShader <- createShader LL.FragmentShader fragmentSrc
  prog <- createProgram [vertexShader, fragmentShader]
  LL.useProgram prog
  mapM_ LL.deleteShader ([vertexShader, fragmentShader] :: [LL.Shader])
  return prog

data Texture2DParams =
  Texture2DParams { wrapS :: LL.TextureWrapParameter
                  , wrapT :: LL.TextureWrapParameter
                  , minFilter :: LL.TextureMinFilter
                  , magFilter :: LL.TextureMagFilter
                  }

instance Default Texture2DParams where
  def = Texture2DParams { wrapS = LL.ClampToEdge
                        , wrapT = LL.ClampToEdge
                        , minFilter = LL.MinLinear
                        , magFilter = LL.MagLinear
                        }

load2DTexture :: Texture2DParams -> FilePath -> IO LL.Texture
load2DTexture Texture2DParams{..} file = do
  tex <- LL.genTexture
  LL.bindTexture LL.Texture2D tex
  LL.texParameter LL.Texture2D (LL.TextureWrapS wrapS)
  LL.texParameter LL.Texture2D (LL.TextureWrapT wrapT)
  LL.texParameter LL.Texture2D (LL.TextureMinFilter minFilter)
  LL.texParameter LL.Texture2D (LL.TextureMagFilter magFilter)
  texImage2D file
  LL.generateMipmap LL.Texture2D
  LL.unbindTexture LL.Texture2D
  return $ tex

data Angle = Deg Float
           | Rad Float

toRad :: Angle -> Float
toRad (Deg a) = a * pi / 180
toRad (Rad a) = a

rotationMatrix :: Angle -> V3 Float -> M44 Float
rotationMatrix angle axis =
  m33_to_m44 $ fromQuaternion $ axisAngle axis (toRad angle)

translationMatrix :: V3 Float -> M44 Float
translationMatrix vec =
  identity & translation .~ vec

perspectiveMatrix :: Angle -> Float -> Float -> Float -> M44 Float
perspectiveMatrix fov aspect near far =
  perspective (toRad fov) aspect near far

uniform2DTexture :: LL.Texture -> LL.TextureUnit -> LL.UniformLocation -> IO ()
uniform2DTexture tex unit loc = do
  LL.activeTexture unit
  LL.bindTexture LL.Texture2D tex
  LL.uniform1i loc (fromIntegral $ fromEnum unit)
