{-# LANGUAGE ScopedTypeVariables #-}
module GLWrap.LowLevel
  ( Shader
  , ShaderType(..)
  , getShader'compileStatus
  , getShader'infoLogLength
  , getShaderInfoLog
  , shaderSource
  , createShader
  , compileShader
  ) where

import Foreign.Ptr
import Graphics.GL.Core33
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek)
import Foreign.Marshal.Utils (with)
import Data.ByteString
import qualified Data.ByteString.Unsafe as BU

newtype Shader = Shader GLuint

data ShaderType = VertexShader
                | FragmentShader
                deriving (Eq, Ord, Show)

getShaderiv :: Shader -> GLenum -> IO GLint
getShaderiv (Shader shaderId) param = do
  alloca $ \(buf :: Ptr GLint) -> do
    glGetShaderiv shaderId param buf
    peek buf

getShader'compileStatus :: Shader -> IO Bool
getShader'compileStatus s = do
  success <- getShaderiv s GL_COMPILE_STATUS
  case fromIntegral success of
    GL_TRUE -> return True
    GL_FALSE -> return False

getShader'infoLogLength :: Num a => Shader -> IO a
getShader'infoLogLength s = fromIntegral <$> getShaderiv s GL_INFO_LOG_LENGTH

getShaderInfoLog :: Shader -> IO ByteString
getShaderInfoLog shader@(Shader shaderId) = do
      logLen <- getShader'infoLogLength shader
      alloca $ \gotBytes -> do
        allocaArray logLen $ \ptr -> do
          glGetShaderInfoLog shaderId (fromIntegral logLen) gotBytes ptr
          reallyGot <- peek gotBytes
          packCStringLen (ptr, fromIntegral reallyGot)

shaderSource :: Shader -> ByteString -> IO ()
shaderSource (Shader shaderId) src =
  BU.unsafeUseAsCStringLen src $ \(ptr, size) ->
    with ptr $ \srcPtrBuf ->
      with (fromIntegral size) $ \sizeBuf ->
        glShaderSource shaderId 1 srcPtrBuf sizeBuf

createShader :: ShaderType -> IO Shader
createShader t = Shader <$> glCreateShader rawT
  where rawT = case t of
                 VertexShader -> GL_VERTEX_SHADER
                 FragmentShader -> GL_FRAGMENT_SHADER

compileShader :: Shader -> IO ()
compileShader (Shader shaderId) = glCompileShader shaderId
