{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW

import System.IO
import System.Exit
import Data.IORef (newIORef, readIORef, IORef)
import Text.RawString.QQ
import qualified Data.ByteString as B
import Control.Exception (finally)

import qualified GLWrap as GL

data AppState = AppState { _window :: GLFW.Window
                         , triangleVAO :: GL.VertexArray
                         , shaderProgram :: GL.Program
                         }


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
keyCallback st window key scancode action mode =
  case key of
    GLFW.Key'Escape ->
      GLFW.setWindowShouldClose window True
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
  GL.useProgram shaderProgram
  GL.bindVertexArray triangleVAO
  -- GL.polygonMode GL.FaceBoth GL.PolyLine
  GL.drawElements GL.TypeTriangles 6 GL.ElementGLuint
  GL.unbindVertexArray
  return ()

fragmentShaderSrc :: B.ByteString
fragmentShaderSrc = [r|#version 330 core
out vec4 color;

void main()
{
    color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
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

initializeApp :: GLFW.Window -> IO AppState
initializeApp window = do
  vao <- GL.genVertexArray
  vbo <- GL.genBuffer
  ebo <- GL.genBuffer
  GL.bindVertexArray vao

  GL.bindBuffer GL.TargetArray vbo
  GL.floatBufferData GL.TargetArray GL.UsageStaticDraw
    [  0.5,  0.5, 0.0
    ,  0.5, -0.5, 0.0
    , -0.5, -0.5, 0.0
    , -0.5,  0.5, 0.0
    ]

  GL.bindBuffer GL.TargetElementArray ebo
  GL.uintBufferData GL.TargetElementArray GL.UsageStaticDraw
    [ 0, 1, 2
    , 0, 2, 3
    ]

  GL.vertexAttribPointer 0 3 GL.AttribPointerFloat False 12 0
  GL.enableVertexAttribArray 0
  GL.unbindVertexArray

  vertexShader <- GL.createShader GL.VertexShader vertexShaderSrc
  fragmentShader <- GL.createShader GL.FragmentShader fragmentShaderSrc
  prog <- GL.createProgram [vertexShader, fragmentShader]
  GL.useProgram prog
  mapM_ GL.deleteShader ([vertexShader, fragmentShader] :: [GL.Shader])

  return $ AppState { _window = window
                    , triangleVAO = vao
                    , shaderProgram = prog
                    }
