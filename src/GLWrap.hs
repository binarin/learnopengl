{-# LANGUAGE ScopedTypeVariables #-}
module GLWrap ( createShader
              , Shader
              , ShaderType(..)
              , ShaderError
              ) where

import Graphics.GL.Core33
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek)
import Foreign.Ptr
import Data.ByteString
import qualified Data.Text as T
import Data.Typeable
import Control.Exception
import Data.Monoid
import Data.Text.Encoding (decodeUtf8With)

import qualified GLWrap.LowLevel as LL
import GLWrap.LowLevel (Shader, ShaderType)

data ShaderError = ShaderError T.Text deriving (Typeable)
instance Exception ShaderError
instance Show ShaderError where
  show (ShaderError t) = "Shader compilation failed: " <> T.unpack t

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
