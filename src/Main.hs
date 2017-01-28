{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE RecordWildCards, QuasiQuotes, OverloadedStrings, DuplicateRecordFields #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW

import System.IO
import System.Exit
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Text.RawString.QQ
import qualified Data.ByteString as B
import Control.Exception (finally)
import Control.Monad (when)
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Control.Concurrent.STM as STM

import qualified GLWrap as GL
import Hello
import Textures
import Behaviour

type EventQueue = TQueue.TQueue InputEvent

data AppState = AppState { _window :: GLFW.Window
                         , input :: EventQueue
                         , states :: [IO Behaviour]
                         , current :: Behaviour
                         }

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
      GLFW.setKeyCallback window $ Just $ keyCallback (input appState)
      mainLoop appStateRef
      return ()
  where
    exitWithErr err = do
      putStrLn err
      exitFailure

initializeApp :: GLFW.Window -> IO AppState
initializeApp window = do
  let r:rs = supportedRenderers
  input <- TQueue.newTQueueIO
  behaviour <- r
  return $ AppState { _window = window
                    , states = rs ++ [r]
                    , current = behaviour
                    , input = input
                    }

oldRendererToBehaviour :: IO (IO (), IO ()) -> IO Behaviour
oldRendererToBehaviour act = do
  (render, cleanup) <- act
  return $ Behaviour { eventFun = \_ -> return ()
                     , renderFun = do
                         GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
                         GL.clear [GL.ClearColor]
                         render
                     , cleanupFun = cleanup
                     }

supportedRenderers = map oldRendererToBehaviour
  [ texturedRectangle
  , triangleWithPerVertexColor
  , triangleColorFromUniform
  , triangleUsingArray
  , doubleTrianglesArray
  , rectangleUsingElements
  , doubleTrianglesDifferentArrays
  , doubleTrianglesDifferentColors
  ]

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

keyCallback :: EventQueue -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback q window key scancode action mode =
  STM.atomically $ TQueue.writeTQueue q $ KeyEvent key scancode action mode

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
    handleInput st
    render st
    GLFW.swapBuffers window
    mainLoop st
  else return ()

handleInput :: IORef AppState -> IO ()
handleInput stRef = do
  st@(AppState{input, current}) <- readIORef stRef
  nextSt <- go st
  writeIORef stRef nextSt
  where
    go st@(AppState{input, current}) = do
      maybeEvent <- STM.atomically $ TQueue.tryReadTQueue input
      case maybeEvent of
        Nothing ->
          return st
        Just event -> do
          nextSt <- handleEvent st event
          go nextSt


handleEvent :: AppState -> InputEvent -> IO AppState
handleEvent st (KeyEvent GLFW.Key'Escape _ _ _) = do
  GLFW.setWindowShouldClose (_window st) True
  return st
handleEvent st (KeyEvent GLFW.Key'Space _ action _) = do
  case action of
    GLFW.KeyState'Released -> do
      cleanupFun $ current st
      initializeNextState st
    _ -> return st
handleEvent st event = do
  eventFun (current st) event
  return st

initializeNextState st@(AppState {current, states = s:ss}) = do
  current' <- s
  return $ st { current = current', states = ss ++ [s] }

render :: IORef AppState -> IO ()
render st = do
  AppState{..} <- readIORef st
  renderFun current
  return ()

errorCb err desc = do
  putStrLn desc
