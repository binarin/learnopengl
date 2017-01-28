{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW

import System.IO
import System.Exit
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Text.RawString.QQ
import qualified Data.ByteString as B
import Control.Exception (finally)
import Control.Monad (when)

import qualified GLWrap as GL

data AppState = AppState { _window :: GLFW.Window
                         , states :: [IO (IO (), IO ())]
                         , renderFun :: IO ()
                         , cleanupFun :: IO ()
                         }


triangleWithPerVertexColor :: IO (IO (), IO ())
triangleWithPerVertexColor = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0, 1, 0, 0
    , -0.5, -0.5, 0.0, 0, 1, 0
    ,    0,  0.5, 0.0, 0, 0, 1
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 24 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 3 GL.AttribPointerFloat False 24 12
  GL.enableVertexAttribArray 1
  GL.unbindVertexArray

  prog <- stdProgram
    [r|#version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec3 color;
      out vec3 ourColor;
      uniform float xOffset;
      void main() {
        gl_Position = vec4(position.x + xOffset, -position.y, position.z, 1.0);
        ourColor = color;
      }
      |]
    [r|#version 330 core
      in vec3 ourColor;
      out vec4 color;
      void main() {
        color = vec4(ourColor, 1.0);
      }
      |]

  offsetLocation <- GL.getUniformLocation prog "xOffset"

  let render = do
        GL.useProgram prog
        GL.uniform1f offsetLocation (0.3 :: Float)
        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)


triangleColorFromUniform :: IO (IO (), IO ())
triangleColorFromUniform = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- stdProgram vertexShaderSrc
    [r|#version 330 core
      out vec4 color;
      uniform vec4 ourColor;
      void main() {
        color = ourColor;
      }
      |]

  let calcGreenValue = do
        maybeTime <- GLFW.getTime
        case maybeTime of
          Nothing -> return 0
          Just time -> return $ (sin(time) / 2) + 0.5

  vertexColorLocation <- GL.getUniformLocation prog "ourColor"

  let render = do
        GL.useProgram prog
        greenValue <- calcGreenValue
        GL.uniform4f vertexColorLocation 0 greenValue 0 1

        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)


triangleUsingArray :: IO (IO (), IO ())
triangleUsingArray = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)

doubleTrianglesArray :: IO (IO (), IO ())
doubleTrianglesArray = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ,  0.7,  0.7, 0.0
    ,  0.8,  0.7, 0.0
    ,  0.8,  0.6, 0.0
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao
        GL.drawArrays GL.TypeTriangles 0 6
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo]
        GL.deleteProgram prog

  return (render, cleanup)


doubleTrianglesDifferentArrays :: IO (IO (), IO ())
doubleTrianglesDifferentArrays = do
  [vao1, vao2] <- GL.genVertexArrays 2
  [vbo1, vbo2] <- GL.genBuffers 2

  GL.bindVertexArray vao1
  GL.bindBuffer GL.TargetArray vbo1
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  GL.bindVertexArray vao2
  GL.bindBuffer GL.TargetArray vbo2
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.7,  0.7, 0.0
    ,  0.8,  0.7, 0.0
    ,  0.8,  0.6, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog <- stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao1
        GL.drawArrays GL.TypeTriangles 0 3
        GL.bindVertexArray vao2
        GL.drawArrays GL.TypeTriangles 0 3
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao1, vao2]
        GL.deleteBuffers [vbo1, vbo2]
        GL.deleteProgram prog

  return (render, cleanup)


doubleTrianglesDifferentColors :: IO (IO (), IO ())
doubleTrianglesDifferentColors = do
  [vao1, vao2] <- GL.genVertexArrays 2
  [vbo1, vbo2] <- GL.genBuffers 2

  GL.bindVertexArray vao1
  GL.bindBuffer GL.TargetArray vbo1
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    ,    0,  0.5, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  GL.bindVertexArray vao2
  GL.bindBuffer GL.TargetArray vbo2
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.7,  0.7, 0.0
    ,  0.8,  0.7, 0.0
    ,  0.8,  0.6, 0.0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  prog1 <- stdProgram vertexShaderSrc fragmentShaderSrc
  prog2 <- stdProgram vertexShaderSrc redFragmentShaderSrc

  let render = do
        GL.useProgram prog1
        GL.bindVertexArray vao1
        GL.drawArrays GL.TypeTriangles 0 3

        GL.useProgram prog2
        GL.bindVertexArray vao2
        GL.drawArrays GL.TypeTriangles 0 3

        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao1, vao2]
        GL.deleteBuffers [vbo1, vbo2]
        GL.deleteProgram prog1
        GL.deleteProgram prog2

  return (render, cleanup)



rectangleUsingElements :: IO (IO (), IO ())
rectangleUsingElements = do
  [vbo, ebo] <- GL.genBuffers 2
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5,  0
    ,  0.5, -0.5,  0
    , -0.5, -0.5,  0
    , -0.5,  0.5,  0
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0

  GL.bindBuffer GL.TargetElementArray ebo
  GL.uintBufferData GL.TargetElementArray GL.UsageStaticDraw
    [ 0, 1, 2
    , 0, 2, 3
    ]

  GL.unbindVertexArray

  prog <- stdProgram vertexShaderSrc fragmentShaderSrc

  let render = do
        GL.useProgram prog
        GL.bindVertexArray vao
        GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint
        GL.unbindVertexArray

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo, ebo]
        GL.deleteProgram prog
        return ()

  return (render, cleanup)

texturedRectangle :: IO (IO (), IO ())
texturedRectangle = do
  [vbo, ebo] <- GL.genBuffers 2
  vao <- GL.genVertexArray

  GL.bindVertexArray vao
  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5,  0,     1, 0, 0,   1, 1
    ,  0.5, -0.5,  0,     0, 1, 0,   1, 0
    , -0.5, -0.5,  0,     0, 0, 1,   0, 0
    , -0.5,  0.5,  0,     1, 1, 0,   0, 1
    ]
  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 32 0
  GL.enableVertexAttribArray 0

  GL.vertexAttribPointer 1 3 GL.AttribPointerFloat False 32 12
  GL.enableVertexAttribArray 1

  GL.vertexAttribPointer 2 2 GL.AttribPointerFloat False 32 24
  GL.enableVertexAttribArray 2

  GL.bindBuffer GL.TargetElementArray ebo
  GL.uintBufferData GL.TargetElementArray GL.UsageStaticDraw
    [ 0, 1, 2
    , 0, 2, 3
    ]

  GL.unbindVertexArray

  [texCont, texFace] <- GL.genTextures 2

  GL.activeTexture GL.Texture0
  GL.bindTexture GL.Texture2D texCont
  GL.texParameter GL.Texture2D (GL.TextureWrapS GL.Repeat)
  GL.texParameter GL.Texture2D (GL.TextureWrapT GL.Repeat)
  GL.texParameter GL.Texture2D (GL.TextureMinFilter GL.MinLinear)
  GL.texParameter GL.Texture2D (GL.TextureMagFilter GL.MagLinear)
  GL.texImage2D "container.jpg"
  GL.generateMipmap GL.Texture2D
  GL.unbindTexture GL.Texture2D

  prog <- stdProgram
    [r|#version 330 core
      layout (location = 0) in vec3 position;
      layout (location = 1) in vec3 color;
      layout (location = 2) in vec2 texCoord;
      out vec3 ourColor;
      out vec2 TexCoord;
      void main() {
        gl_Position = vec4(position, 1.0f);
        ourColor = color;
        TexCoord = texCoord;
      }
      |]
    [r|#version 330 core
      in vec3 ourColor;
      in vec2 TexCoord;
      out vec4 color;
      uniform sampler2D ourTexture;
      void main() {
        color = texture(ourTexture, TexCoord) * vec4(ourColor, 1.0f);
      }
      |]

  let render = do
        GL.useProgram prog
        GL.bindTexture GL.Texture2D texCont
        GL.bindVertexArray vao
        GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint
        GL.unbindVertexArray
        GL.unbindTexture GL.Texture2D

  let cleanup = do
        GL.deleteVertexArrays [vao]
        GL.deleteBuffers [vbo, ebo]
        GL.deleteProgram prog
        GL.deleteTextures [texCont, texFace]
        return ()

  return (render, cleanup)


stdProgram :: B.ByteString -> B.ByteString -> IO GL.Program
stdProgram vertexSrc fragmentSrc = do
  vertexShader <- GL.createShader GL.VertexShader vertexSrc
  fragmentShader <- GL.createShader GL.FragmentShader fragmentSrc
  prog <- GL.createProgram [vertexShader, fragmentShader]
  GL.useProgram prog
  mapM_ GL.deleteShader ([vertexShader, fragmentShader] :: [GL.Shader])
  return prog

boolBracket :: IO Bool -> IO () -> IO a -> IO a -> IO a
boolBracket action cleanup onErr onSuccess = do
  success <- action
  case success of
    True -> onSuccess `finally` cleanup
    False -> onErr

maybeBracket :: IO (Maybe a) -> (a -> IO ()) -> IO b -> (a -> IO b) -> IO b
maybeBracket action cleanup onErr onSuccess = do
  result <- action
  case result of
    Nothing -> onErr
    Just val -> onSuccess val `finally` cleanup val

main :: IO ()
main = do
  GLFW.setErrorCallback $ Just errorCb
  boolBracket GLFW.init GLFW.terminate (exitWithErr "init failed") $ do
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    let createAction = GLFW.createWindow 800 600 "LearnOpenGL" Nothing Nothing
    maybeBracket createAction GLFW.destroyWindow (exitWithErr "failed to create window") $ \window -> do
      GLFW.makeContextCurrent $ Just window
      (width, height) <- GLFW.getFramebufferSize window
      GL.viewport (GL.WinCoord 0) (GL.WinCoord 0) (GL.toWidth width) (GL.toHeight height)
      appState <- initializeApp window
      appStateRef <- newIORef appState
      GLFW.setKeyCallback window $ Just $ keyCallback appStateRef
      mainLoop appStateRef
      return ()

  where
    exitWithErr err = do
      putStrLn err
      exitFailure

keyCallback :: IORef AppState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback stRef window key scancode action mode =
  case key of
    GLFW.Key'Escape ->
      GLFW.setWindowShouldClose window True
    GLFW.Key'Space ->
      when (action == GLFW.KeyState'Released) $ rotateRender stRef
    _ ->
      return ()

appWindow :: IORef AppState -> IO GLFW.Window
appWindow stateRef = do
  AppState{_window = window} <- readIORef stateRef
  return window

mainLoop :: IORef AppState -> IO ()
mainLoop st = do
  window <- appWindow st
  shouldClose <- appWindow st >>= GLFW.windowShouldClose
  if not shouldClose then do
    GLFW.pollEvents
    render st
    GLFW.swapBuffers window
    mainLoop st
  else return ()

render :: IORef AppState -> IO ()
render st = do
  AppState{..} <- readIORef st
  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor]
  renderFun
  return ()

fragmentShaderSrc :: B.ByteString
fragmentShaderSrc = [r|#version 330 core
out vec4 color;

void main()
{
    color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
}
|]

redFragmentShaderSrc :: B.ByteString
redFragmentShaderSrc = [r|#version 330 core
out vec4 color;

void main()
{
    color = vec4(1.0f, 0.0f, 0.0f, 1.0f);
}
|]


vertexShaderSrc :: B.ByteString
vertexShaderSrc = [r|#version 330 core
layout (location = 0) in vec3 position;

void main()
{
  gl_Position = vec4(position.x, position.y, position.z, 1.0);
}
|]

errorCb err desc = do
  putStrLn desc


supportedRenderers = [ texturedRectangle
                     , triangleWithPerVertexColor
                     , triangleColorFromUniform
                     , triangleUsingArray
                     , doubleTrianglesArray
                     , rectangleUsingElements
                     , doubleTrianglesDifferentArrays
                     , doubleTrianglesDifferentColors
                     ]

initializeApp :: GLFW.Window -> IO AppState
initializeApp window = do
  let r:rs = supportedRenderers
  (render, cleanup) <- r
  return $ AppState { _window = window
                    , states = rs ++ [r]
                    , renderFun = render
                    , cleanupFun = cleanup
                    }

rotateRender :: IORef AppState -> IO ()
rotateRender stRef = do
  st@(AppState{cleanupFun, states = s:ss}) <- readIORef stRef
  cleanupFun
  (newRender, newCleanup) <- s
  let newStates = ss ++ [s]
  writeIORef stRef $ st { renderFun = newRender
                        , cleanupFun = newCleanup
                        , states = newStates
                        }
