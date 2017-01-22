{-# LANGUAGE ScopedTypeVariables #-}
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
              , LL.DrawElementType(..)
              , LL.drawElements
              , LL.MaterialFace(..)
              , LL.PolygonMode(..)
              , LL.polygonMode
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
import Control.Monad
import Data.Monoid
import Data.Text.Encoding (decodeUtf8With)

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
