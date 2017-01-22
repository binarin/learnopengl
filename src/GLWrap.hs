{-# LANGUAGE ScopedTypeVariables #-}
module GLWrap ( getShader'infoLogLength
              , getShader'compileStatus
              , createShader
              , Shader(..)
              ) where

import Graphics.GL.Core33
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Data.ByteString
import qualified Data.ByteString.Unsafe as BU


data ShaderType = VertexShader
                | FragmentShader
                deriving (Eq, Ord, Show)

newtype Shader = Shader GLuint

shaderSource :: Shader -> ByteString -> IO ()
shaderSource (Shader shaderId) src =
  BU.unsafeUseAsCStringLen src $ \(ptr, size) ->
    with ptr $ \srcPtrBuf ->
      with (fromIntegral size) $ \sizeBuf ->
        glShaderSource shaderId 1 srcPtrBuf sizeBuf


compileShader :: Shader -> IO (Either ByteString ())
compileShader shader@(Shader shaderId) = do
  glCompileShader shaderId
  status <- getShader'compileStatus shader
  case status of
    True -> return $ Right ()
    False -> do
      logLen <- getShader'infoLogLength shader
      alloca $ \gotBytes -> do
        allocaArray logLen $ \ptr -> do
          glGetShaderInfoLog shaderId (fromIntegral logLen) gotBytes ptr
          reallyGot <- peek gotBytes
          Left <$> packCStringLen (ptr, fromIntegral reallyGot)


createShader :: ShaderType -> ByteString -> IO (Either ByteString Shader)
createShader shaderType src = do
  shader <- Shader <$> glCreateShader shaderTypeRaw
  shaderSource shader src
  fmap (const shader) <$> compileShader shader
  where
    shaderTypeRaw = case shaderType of
                      VertexShader -> GL_VERTEX_SHADER
                      FragmentShader -> GL_FRAGMENT_SHADER

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
