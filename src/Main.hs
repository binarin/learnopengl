{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

import System.IO
import System.Exit
import System.Environment
import Data.IORef (newIORef, readIORef, IORef)

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x

errorCb err desc = do
  putStrLn desc

data AppState = AppState { _window :: GLFW.Window }

main :: IO ()
main = do
  GLFW.setErrorCallback $ Just errorCb
  successfulInit <- GLFW.init
  bool successfulInit exitFailure $ do
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    mw <- GLFW.createWindow 800 600 "LearnOpenGL" Nothing Nothing
    maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
      GLFW.makeContextCurrent mw
      (width, height) <- GLFW.getFramebufferSize window
      glViewport (fromIntegral width) (fromIntegral height) 0 0
      appState <- newIORef $ AppState window
      GLFW.setKeyCallback window $ Just $ keyCallback appState
      mainLoop appState
      GLFW.destroyWindow window
      exitSuccess

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
