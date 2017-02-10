module Behaviour where

import qualified Graphics.UI.GLFW as GLFW
import qualified GLWrap as GL
import qualified Data.Set as Set

import Data.IORef (readIORef, modifyIORef, newIORef)

type Scancode = Int

data InputEvent = KeyEvent GLFW.Key Scancode GLFW.KeyState GLFW.ModifierKeys
                | MouseEvent Float Float
  deriving (Show)

type Keys = Set.Set GLFW.Key

data Behaviour = Behaviour { frameFun :: [InputEvent] -> Float -> IO ()
                           , renderFun :: GL.Width -> GL.Height -> IO ()
                           , cleanupFun :: IO ()
                           }

trackKeys :: Keys -> InputEvent -> Keys
trackKeys keys (KeyEvent key _ action _) = modifySet key keys
  where modifySet = case action of
          GLFW.KeyState'Released -> Set.delete
          _ -> Set.insert
trackKeys keys _ = keys

type InitFun a = IO a
type FrameFun a = [InputEvent] -> Float -> a -> a
type RenderFun a = a -> GL.Width -> GL.Height -> IO ()
type CleanupFun a = a -> IO ()

mkBehaviour :: InitFun a -> FrameFun a -> RenderFun a -> CleanupFun a -> IO Behaviour
mkBehaviour init frame render cleanup = do
  state <- init
  stateRef <- newIORef state
  let bFrame events time = do
        modifyIORef stateRef $ frame events time
  let bRender width height = do
        state <- readIORef stateRef
        render state width height
  let bCleanup = do
        state <- readIORef stateRef
        cleanup state
  return $ Behaviour { frameFun = bFrame, renderFun = bRender, cleanupFun = bCleanup }
