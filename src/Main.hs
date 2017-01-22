{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

import Control.Monad (when)
import System.IO
import System.Exit
import System.Environment
import Data.IORef (newIORef, readIORef, IORef)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import Data.Array.Storable (withStorableArray)
import Data.Array.MArray (newListArray)
import Text.RawString.QQ
import Foreign.Ptr
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString as B
import Control.Exception (bracket, finally)

import qualified GLWrap as GL

data AppState = AppState { _window :: GLFW.Window }


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
      glViewport (fromIntegral width) (fromIntegral height) 0 0
      appState <- newIORef $ AppState window
      GLFW.setKeyCallback window $ Just $ keyCallback appState
      mainLoop appState
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
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT
  vbo <- alloca $ \buf -> do
    glGenBuffers 1 buf
    peek buf
  glBindBuffer GL_ARRAY_BUFFER vbo
  bufferData firstTriangle
  vertexShader <- GL.createShader GL.VertexShader vertexShaderSrc
  fragmentShader <- GL.createShader GL.FragmentShader fragmentShaderSrc
  prog <- GL.createProgram [vertexShader, fragmentShader]
  GL.useProgram prog
  mapM_ GL.deleteShader [vertexShader, fragmentShader]
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

bufferData :: [GLfloat] -> IO ()
bufferData lst = do
  let len = length lst
  arr <- newListArray (0, len - 1) lst
  withStorableArray arr $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ 4 * len) ptr GL_STATIC_DRAW
  return ()


firstTriangle = [ -0.5, -0.5, 0.0
                ,  0.5, -0.5, 0.0
                ,  0.0,  0.5, 0.0
                ]

errorCb err desc = do
  putStrLn desc
