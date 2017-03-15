{-# LANGUAGE ScopedTypeVariables #-}
module GLWrap.LowLevel where

import Debug.Trace
import Data.Bits
import Foreign.Ptr
import Graphics.GL.Core33
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Data.Array.MArray (newListArray, getElems, readArray, writeArray)
import Data.Array.Storable (withStorableArray)
import Foreign.Storable (peek, Storable)
import Foreign.Marshal.Utils (with)
import Data.ByteString hiding (head, length, map, foldr, transpose)
import qualified Data.ByteString.Unsafe as BU
import GHC.Float (double2Float)
import qualified Data.Vector.Storable as VS
import Linear.Matrix

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

bufferData4byte :: (Storable a, Show a, Num a) => BufferTarget -> BufferUsage -> [a] -> IO ()
bufferData4byte tgt usage lst = do
  let len = length lst
  -- XXX additional element added - need to check on non-intel
  -- hardware whether it's my bug or driver bug
  arr <- newListArray (0, len) lst
  withStorableArray arr $ \ptr -> do
    glBufferData (serializeTarget tgt) (fromIntegral $ 4 * (len + 1)) ptr (serializeUsage usage)

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

newtype WinCoord = WinCoord GLint deriving (Show)
newtype Width = Width GLsizei deriving (Show)
newtype Height = Height GLsizei deriving (Show)

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

deleteVertexArrays :: [VertexArray] -> IO ()
deleteVertexArrays vaos = do
  let len = length vaos
  let vaosWithConstructorStripped = map (\(VertexArray vao) -> vao) vaos
  arr <- newListArray (0, len - 1) vaosWithConstructorStripped
  withStorableArray arr $ \ptr ->
    glDeleteVertexArrays (fromIntegral len) ptr

deleteBuffers :: [Buffer] -> IO ()
deleteBuffers vbos = do
  let len = length vbos
  let vbosWithConstructorStripped = map (\(Buffer vbo) -> vbo) vbos
  arr <- newListArray (0, len - 1) vbosWithConstructorStripped
  withStorableArray arr $ \ptr ->
    glDeleteBuffers (fromIntegral len) ptr

deleteProgram :: Program -> IO ()
deleteProgram (Program pid) = glDeleteProgram pid

newtype UniformLocation = UniformLocation GLint

getUniformLocation :: Program -> ByteString -> IO (Maybe UniformLocation)
getUniformLocation (Program pid) name = do
  ret <- useAsCString name $ \ptr -> glGetUniformLocation pid ptr
  case ret of
    -1 -> return $ Nothing
    _ -> return $ Just $ UniformLocation ret

class Floaty a where
  toFloat :: a -> Float

instance Floaty Float where
  toFloat = id

instance Floaty Double where
  toFloat = double2Float

uniform4f :: Floaty a => UniformLocation -> a -> a -> a -> a -> IO ()
uniform4f (UniformLocation loc) x y z w = glUniform4f loc (toFloat x) (toFloat y) (toFloat z) (toFloat w)

uniform3f :: Floaty a => UniformLocation -> a -> a -> a -> IO ()
uniform3f (UniformLocation loc) x y z = glUniform3f loc (toFloat x) (toFloat y) (toFloat z)

uniform1f :: Floaty a => UniformLocation -> a -> IO ()
uniform1f (UniformLocation loc) x = glUniform1f loc (toFloat x)

uniform1i :: UniformLocation -> GLint -> IO ()
uniform1i (UniformLocation loc) x = glUniform1i loc x

newtype Texture = Texture GLuint

genTexture :: IO Texture
genTexture = head <$> genTextures 1

genTextures :: Integral a => a -> IO [Texture]
genTextures count = do
  textures <- allocaArray (fromIntegral count) $ \buf -> do
    glGenTextures (fromIntegral count) buf
    peekArray (fromIntegral count) buf
  return $ fmap Texture textures

data TextureTarget = Texture1D | Texture2D | Texture3D

serializeTextureTarget :: TextureTarget -> GLenum
serializeTextureTarget Texture1D = GL_TEXTURE_1D
serializeTextureTarget Texture2D = GL_TEXTURE_2D
serializeTextureTarget Texture3D = GL_TEXTURE_3D

instance ToGL TextureTarget where
  toGLenum = serializeTextureTarget

data InternalFormat = InternalFormatRGB

serializeInternalFormat :: InternalFormat -> GLint
serializeInternalFormat InternalFormatRGB = fromIntegral GL_RGB

data PixelFormat = PixelRGB

serializePixelFormat :: PixelFormat -> GLenum
serializePixelFormat PixelRGB = GL_RGB

data PixelType = PixelGLubyte

serializePixelType :: PixelType -> GLenum
serializePixelType PixelGLubyte = GL_UNSIGNED_BYTE

bindTexture :: TextureTarget -> Texture -> IO ()
bindTexture target (Texture tid) = glBindTexture (serializeTextureTarget target) tid

texImage2D :: Storable a => (Integral lod) => TextureTarget -> lod -> InternalFormat -> Width -> Height -> PixelFormat -> PixelType -> VS.Vector a -> IO ()
texImage2D target lod intFmt (Width w) (Height h) pixFmt pixType vec = do
  VS.unsafeWith vec $ \ptr -> do
    glTexImage2D (serializeTextureTarget target) (fromIntegral lod) (serializeInternalFormat intFmt) w h 0 (serializePixelFormat pixFmt) (serializePixelType pixType) ptr

deleteTextures :: [Texture] -> IO ()
deleteTextures textures = do
  let len = length textures
  let texturesWithConstructorStripped = map (\(Texture texture) -> texture) textures
  arr <- newListArray (0, len - 1) texturesWithConstructorStripped
  withStorableArray arr $ \ptr ->
    glDeleteTextures (fromIntegral len) ptr

unbindTexture :: TextureTarget -> IO ()
unbindTexture tgt = glBindTexture (serializeTextureTarget tgt) 0


class ToGL a where
  toGLenum :: a -> GLenum

toGL :: (ToGL a, Integral b) => a -> b
toGL a = fromIntegral $ toGLenum a

data TextureWrapParameter = ClampToEdge
                          | ClampToBorder RGBA
                          | MirroredRepeat
                          | Repeat

instance ToGL TextureWrapParameter where
  toGLenum ClampToEdge = GL_CLAMP_TO_EDGE
  toGLenum (ClampToBorder _) = GL_CLAMP_TO_BORDER
  toGLenum MirroredRepeat = GL_MIRRORED_REPEAT
  toGLenum Repeat = GL_REPEAT

data TextureMinFilter = MinNearest
                      | MinLinear
                      | MinNearestMipmapNearest
                      | MinNearestMipmapLinear
                      | MinLinearMipmapNearest
                      | MinLinearMipmapLinear

instance ToGL TextureMinFilter where
  toGLenum MinNearest = GL_NEAREST
  toGLenum MinLinear = GL_LINEAR
  toGLenum MinNearestMipmapNearest = GL_NEAREST_MIPMAP_NEAREST
  toGLenum MinNearestMipmapLinear = GL_NEAREST_MIPMAP_LINEAR
  toGLenum MinLinearMipmapNearest = GL_LINEAR_MIPMAP_NEAREST
  toGLenum MinLinearMipmapLinear = GL_LINEAR_MIPMAP_LINEAR

data TextureMagFilter = MagNearest
                      | MagLinear

instance ToGL TextureMagFilter where
  toGLenum MagNearest = GL_NEAREST
  toGLenum MagLinear = GL_LINEAR


data TextureParameter = TextureWrapS TextureWrapParameter
                      | TextureWrapT TextureWrapParameter
                      | TextureMinFilter TextureMinFilter
                      | TextureMagFilter TextureMagFilter

data TextureParameterFV = TextureBorderColor RGBA
instance ToGL TextureParameterFV where
  toGLenum (TextureBorderColor _) = GL_TEXTURE_BORDER_COLOR

texParameterfv :: TextureTarget -> TextureParameterFV -> IO ()
texParameterfv tgt param@(TextureBorderColor (RGBA r g b a)) = do
  arr <- newListArray (0, 3) [r, g, b, a]
  withStorableArray arr $ \ptr ->
    glTexParameterfv (serializeTextureTarget tgt) (toGL param) ptr

data TextureParameterI = TextureIWrapS
                       | TextureIWrapT
                       | TextureIMinFilter
                       | TextureIMagFilter

instance ToGL TextureParameterI where
  toGLenum TextureIWrapS = GL_TEXTURE_WRAP_S
  toGLenum TextureIWrapT = GL_TEXTURE_WRAP_T
  toGLenum TextureIMinFilter = GL_TEXTURE_MIN_FILTER
  toGLenum TextureIMagFilter = GL_TEXTURE_MAG_FILTER

texParameteri :: ToGL val => TextureTarget -> TextureParameterI -> val -> IO ()
texParameteri tgt param value = do
  glTexParameteri (toGL tgt) (toGL param) (toGL value)

texParameter :: TextureTarget -> TextureParameter -> IO ()
texParameter tgt (TextureWrapS (wrap@(ClampToBorder border))) = do
  texParameteri tgt TextureIWrapS wrap
  texParameterfv tgt (TextureBorderColor border)
texParameter tgt (TextureWrapT (wrap@(ClampToBorder border))) = do
  texParameteri tgt TextureIWrapT wrap
  texParameterfv tgt (TextureBorderColor border)
texParameter tgt (TextureWrapS wrap) = texParameteri tgt TextureIWrapS wrap
texParameter tgt (TextureWrapT wrap) = texParameteri tgt TextureIWrapT wrap
texParameter tgt (TextureMinFilter filter) = texParameteri tgt TextureIMinFilter filter
texParameter tgt (TextureMagFilter filter) = texParameteri tgt TextureIMagFilter filter


generateMipmap :: TextureTarget -> IO ()
generateMipmap tgt = glGenerateMipmap (toGL tgt)

data TextureUnit = Texture0
                 | Texture1
                 | Texture2
                 | Texture3
                 | Texture4
                 | Texture5
                 | Texture6
                 | Texture7
                 | Texture8
                 | Texture9
                 | Texture10
                 | Texture11
                 | Texture12
                 | Texture13
                 | Texture14
                 | Texture15
  deriving (Eq, Ord, Enum)

instance ToGL TextureUnit where
  toGLenum Texture0 = GL_TEXTURE0
  toGLenum Texture1 = GL_TEXTURE1
  toGLenum Texture2 = GL_TEXTURE2
  toGLenum Texture3 = GL_TEXTURE3
  toGLenum Texture4 = GL_TEXTURE4
  toGLenum Texture5 = GL_TEXTURE5
  toGLenum Texture6 = GL_TEXTURE6
  toGLenum Texture7 = GL_TEXTURE7
  toGLenum Texture8 = GL_TEXTURE8
  toGLenum Texture9 = GL_TEXTURE9
  toGLenum Texture10 = GL_TEXTURE10
  toGLenum Texture11 = GL_TEXTURE11
  toGLenum Texture12 = GL_TEXTURE12
  toGLenum Texture13 = GL_TEXTURE13
  toGLenum Texture14 = GL_TEXTURE14
  toGLenum Texture15 = GL_TEXTURE15

activeTexture :: TextureUnit -> IO ()
activeTexture unit = glActiveTexture (toGL unit)

uniformMatrix4fv :: UniformLocation -> M44 Float -> IO ()
uniformMatrix4fv (UniformLocation loc) mat =
  with mat $ \ptr ->
    glUniformMatrix4fv loc 1 GL_TRUE (castPtr (ptr :: Ptr (M44 Float)))


data Capability = DepthTest

instance ToGL Capability where
  toGLenum DepthTest = GL_DEPTH_TEST

enable :: Capability -> IO ()
enable cap = glEnable (toGL cap)

disable :: Capability -> IO ()
disable cap = glDisable (toGL cap)
