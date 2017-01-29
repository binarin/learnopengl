module Behaviour where

import qualified Graphics.UI.GLFW as GLFW

import Data.IORef (readIORef, modifyIORef, newIORef)

type Scancode = Int

data InputEvent = KeyEvent GLFW.Key Scancode GLFW.KeyState GLFW.ModifierKeys
  deriving (Show)

data Behaviour = Behaviour { frameFun :: [InputEvent] -> Float -> IO ()
                           , renderFun :: IO ()
                           , cleanupFun :: IO ()
                           }


type InitFun a = IO a
type FrameFun a = [InputEvent] -> Float -> a -> a
type RenderFun a = a -> IO ()
type CleanupFun a = a -> IO ()

mkBehaviour :: InitFun a -> FrameFun a -> RenderFun a -> CleanupFun a -> IO Behaviour
mkBehaviour init frame render cleanup = do
  state <- init
  stateRef <- newIORef state
  let bFrame events time = do
        modifyIORef stateRef $ frame events time
  let bRender = do
        state <- readIORef stateRef
        render state
  let bCleanup = do
        state <- readIORef stateRef
        cleanup state
  return $ Behaviour { frameFun = bFrame, renderFun = bRender, cleanupFun = bCleanup }
