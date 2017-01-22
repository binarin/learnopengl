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
  , Program
  , createProgram
  , attachShader
  , linkProgram
  , getProgram'linkStatus
  , getProgram'infoLogLength
  , getProgramInfoLog
  , useProgram
  , deleteShader
  , AttribPointerType(..)
  , vertexAttribPointer
  , enableVertexAttribArray
  , VertexArray
  , genVertexArray
  , genVertexArrays
  , bindVertexArray
  , unbindVertexArray
  , drawArrays
  , PrimitiveType(..)
  , Buffer
  , genBuffer
  , genBuffers
  , BufferTarget(..)
  , bindBuffer
  , BufferUsage(..)
  , uintBufferData
  , floatBufferData
  , DrawElementType(..)
  , drawElements
  , MaterialFace(..)
  , PolygonMode(..)
  , polygonMode
  , WinCoord(..)
  , Width(..)
  , Height(..)
  , viewport
  , toWinCoord
  , toHeight
  , toWidth
  , RGBA(..)
  , clearColor
  , ClearBufferBit(..)
  , ClearBufferMask
  , clear
  ) where

import Data.Bits
import Foreign.Ptr
import Graphics.GL.Core33
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Data.Array.MArray (newListArray)
import Data.Array.Storable (withStorableArray)
import Foreign.Storable (peek, Storable)
import Foreign.Marshal.Utils (with)
import Data.ByteString hiding (head, length, map, foldr)
import qualified Data.ByteString.Unsafe as BU

newtype Shader = Shader GLuint

data ShaderType = VertexShader
                | FragmentShader
                deriving (Eq, Ord, Show)

newtype Program = Program GLuint

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

createProgram :: IO Program
createProgram = Program <$> glCreateProgram

attachShader :: Program -> Shader -> IO ()
attachShader (Program pid) (Shader sid) = glAttachShader pid sid

linkProgram :: Program -> IO ()
linkProgram (Program pid) = glLinkProgram pid

getProgramiv :: Program -> GLenum -> IO GLint
getProgramiv (Program programId) param = do
  alloca $ \(buf :: Ptr GLint) -> do
    glGetProgramiv programId param buf
    peek buf

getProgram'linkStatus :: Program -> IO Bool
getProgram'linkStatus p = do
  success <- getProgramiv p GL_LINK_STATUS
  case fromIntegral success of
    GL_TRUE -> return True
    GL_FALSE -> return False

getProgram'infoLogLength :: Num a => Program -> IO a
getProgram'infoLogLength p = fromIntegral <$> getProgramiv p GL_INFO_LOG_LENGTH

getProgramInfoLog :: Program -> IO ByteString
getProgramInfoLog prog@(Program pid) = do
      logLen <- getProgram'infoLogLength prog
      alloca $ \gotBytes -> do
        allocaArray logLen $ \ptr -> do
          glGetShaderInfoLog pid (fromIntegral logLen) gotBytes ptr
          reallyGot <- peek gotBytes
          packCStringLen (ptr, fromIntegral reallyGot)

useProgram :: Program -> IO ()
useProgram (Program pid) = glUseProgram pid

deleteShader :: Shader -> IO ()
deleteShader (Shader sid) = glDeleteShader sid

data AttribPointerType = AttribPointerFloat

vertexAttribPointer :: Integral a => GLuint -> GLint -> AttribPointerType -> Bool -> GLsizei -> a -> IO ()
vertexAttribPointer index size typ normalized stride offset = do
  glVertexAttribPointer index size rawType rawNormalized stride offsetAsPtr
  where
    rawType = case typ of
                AttribPointerFloat -> GL_FLOAT
    rawNormalized = case normalized of
      True -> GL_TRUE
      False -> GL_FALSE
    offsetAsPtr = plusPtr nullPtr (fromIntegral offset)

enableVertexAttribArray :: GLuint -> IO ()
enableVertexAttribArray = glEnableVertexAttribArray

newtype VertexArray = VertexArray GLuint

genVertexArray :: IO VertexArray
genVertexArray = head <$> genVertexArrays 1

genVertexArrays :: Integral a => a -> IO [VertexArray]
genVertexArrays count = do
  vaos <- allocaArray (fromIntegral count) $ \buf -> do
    glGenVertexArrays (fromIntegral count) buf
    peekArray (fromIntegral count) buf
  return $ fmap VertexArray vaos

bindVertexArray :: VertexArray -> IO ()
bindVertexArray (VertexArray vao) = glBindVertexArray vao

unbindVertexArray :: IO ()
unbindVertexArray = glBindVertexArray 0

data PrimitiveType = TypeLines | TypeTriangles

serializePrimitiveType :: PrimitiveType -> GLenum
serializePrimitiveType TypeLines = GL_LINES
serializePrimitiveType TypeTriangles = GL_TRIANGLES

drawArrays :: (Integral first, Integral count) => PrimitiveType -> first -> count -> IO ()
drawArrays typ first count = do
  glDrawArrays (serializePrimitiveType typ) (fromIntegral first) (fromIntegral count)

newtype Buffer = Buffer GLuint

genBuffer :: IO Buffer
genBuffer = head <$> genBuffers 1

genBuffers :: Integral a => a -> IO [Buffer]
genBuffers count = do
  vbos <- allocaArray (fromIntegral count) $ \buf -> do
    glGenBuffers (fromIntegral count) buf
    peekArray (fromIntegral count) buf
  return $ fmap Buffer vbos

data BufferTarget = TargetArray | TargetElementArray

serializeTarget :: BufferTarget -> GLenum
serializeTarget TargetArray = GL_ARRAY_BUFFER
serializeTarget TargetElementArray = GL_ELEMENT_ARRAY_BUFFER

bindBuffer :: BufferTarget -> Buffer -> IO ()
bindBuffer tgt (Buffer vbo) = glBindBuffer (serializeTarget tgt) vbo
  where

data BufferUsage = UsageStaticDraw

serializeUsage :: BufferUsage -> GLenum
serializeUsage UsageStaticDraw = GL_STATIC_DRAW

uintBufferData :: BufferTarget -> BufferUsage -> [GLuint] -> IO ()
uintBufferData = bufferData4byte

floatBufferData :: BufferTarget -> BufferUsage -> [GLfloat] -> IO ()
floatBufferData = bufferData4byte

bufferData4byte :: (Storable a) => BufferTarget -> BufferUsage -> [a] -> IO ()
bufferData4byte tgt usage lst = do
  let len = length lst
  arr <- newListArray (0, len - 1) lst
  withStorableArray arr $ \ptr ->
    glBufferData (serializeTarget tgt) (fromIntegral $ 4 * len) ptr (serializeUsage usage)

data DrawElementType = ElementGLuint

serializeDrawElementType ElementGLuint = GL_UNSIGNED_INT

drawElements :: Integral count => PrimitiveType -> count -> DrawElementType -> IO ()
drawElements primType cnt drawType = glDrawElements (serializePrimitiveType primType) (fromIntegral cnt) (serializeDrawElementType drawType) nullPtr


data MaterialFace = FaceBack | FaceFront | FaceBoth

serializeMaterialFace :: MaterialFace -> GLenum
serializeMaterialFace FaceBack = GL_BACK
serializeMaterialFace FaceFront = GL_FRONT
serializeMaterialFace FaceBoth = GL_FRONT_AND_BACK

data PolygonMode = PolyLine | PolyFill | PolyPoint

serializePolygonMode PolyLine = GL_LINE
serializePolygonMode PolyFill = GL_FILL
serializePolygonMode PolyPoint = GL_POINT

polygonMode matFace polyMode =
  glPolygonMode (serializeMaterialFace matFace) (serializePolygonMode polyMode)

newtype WinCoord = WinCoord GLint
newtype Width = Width GLsizei
newtype Height = Height GLsizei

viewport :: WinCoord -> WinCoord -> Width -> Height -> IO ()
viewport (WinCoord x) (WinCoord y) (Width w) (Height h) =
  glViewport x y w h

toWinCoord :: Integral a => a -> WinCoord
toWinCoord a = WinCoord $ fromIntegral a

toWidth :: Integral a => a -> Width
toWidth a = Width $ fromIntegral a

toHeight :: Integral a => a -> Height
toHeight a = Height $ fromIntegral a

data RGBA = RGBA GLfloat GLfloat GLfloat GLfloat

clearColor :: RGBA -> IO ()
clearColor (RGBA r g b a) = glClearColor r g b a

data ClearBufferBit = ClearColor | ClearDepth | ClearStencil

type ClearBufferMask = [ClearBufferBit]

serializeClearBufferBit ClearColor = GL_COLOR_BUFFER_BIT
serializeClearBufferBit ClearDepth = GL_DEPTH_BUFFER_BIT
serializeClearBufferBit ClearStencil = GL_STENCIL_BUFFER_BIT

serializeClearBufferMask = foldr (.|.) 0 . map serializeClearBufferBit

clear :: ClearBufferMask -> IO ()
clear m = glClear (serializeClearBufferMask m)
