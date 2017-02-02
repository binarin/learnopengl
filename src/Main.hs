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
import GHC.Float (double2Float)
import Data.Maybe (fromMaybe)

import qualified GLWrap as GL
import Hello
import Textures
import Behaviour
import Coordinate (staticMatrixStack)

type EventQueue = TQueue.TQueue InputEvent

data AppState = AppState { _window :: GLFW.Window
                         , input :: EventQueue
                         , states :: [IO Behaviour]
                         , current :: Behaviour
                         , screenWidth :: GL.Width
                         , screenHeight :: GL.Height
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
      appState <- initializeApp window width height
      appStateRef <- newIORef appState
      GLFW.setKeyCallback window $ Just $ keyCallback (input appState)
      mainLoop appStateRef
      return ()
  where
    exitWithErr err = do
      putStrLn err
      exitFailure

initializeApp :: GLFW.Window -> Int -> Int -> IO AppState
initializeApp window width height = do
  let r:rs = supportedRenderers
  input <- TQueue.newTQueueIO
  behaviour <- r
  return $ AppState { _window = window
                    , states = rs ++ [r]
                    , current = behaviour
                    , input = input
                    , screenWidth = GL.Width (fromIntegral width)
                    , screenHeight = GL.Height (fromIntegral height)
                    }

oldRendererToBehaviour :: IO (IO (), IO ()) -> IO Behaviour
oldRendererToBehaviour act = do
  (render, cleanup) <- act
  return $ Behaviour { frameFun = \_ _ -> return ()
                     , renderFun = \_ _ -> do
                         GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
                         GL.clear [GL.ClearColor]
                         render
                     , cleanupFun = cleanup
                     }

supportedRenderers =
  [ staticMatrixStack
  , texturedRectangle
  ] ++ oldRenderers

oldRenderers = map oldRendererToBehaviour
  [ triangleWithPerVertexColor
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
  nextSt <- go st []
  writeIORef stRef nextSt
  where
    go (st@(AppState{input, current})) behaviourEvents = do
      maybeEvent <- STM.atomically $ TQueue.tryReadTQueue input
      case maybeEvent of
        Nothing -> do
          callFrameFun (frameFun current) behaviourEvents
          return st
        Just event -> do
          wasHandled <- handleEvent st event
          case wasHandled of
            Nothing ->
              go st (event:behaviourEvents)
            Just nextState ->
              go nextState behaviourEvents

callFrameFun :: ([InputEvent] -> Float -> IO ()) -> [InputEvent] -> IO ()
callFrameFun frameFun events = do
  time <- double2Float . fromMaybe 0 <$> GLFW.getTime
  frameFun events time

handleEvent :: AppState -> InputEvent -> IO (Maybe AppState)
handleEvent st (KeyEvent GLFW.Key'Escape _ _ _) = do
  GLFW.setWindowShouldClose (_window st) True
  return $ Just st
handleEvent st (KeyEvent GLFW.Key'Space _ action _) = do
  case action of
    GLFW.KeyState'Released -> do
      cleanupFun $ current st
      Just <$> initializeNextState st
    _ -> return $ Just st
handleEvent st _ = do
  return Nothing

initializeNextState st@(AppState {current, states = s:ss}) = do
  current' <- s
  return $ st { current = current', states = ss ++ [s] }

render :: IORef AppState -> IO ()
render st = do
  AppState{..} <- readIORef st
  renderFun current screenWidth screenHeight
  return ()

errorCb err desc = do
  putStrLn desc
