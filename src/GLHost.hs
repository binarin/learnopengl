{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module GLHost where

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents, EventTrigger)
import Control.Lens
import Control.Monad.Identity (Identity(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (readIORef, IORef)
import Control.Monad.Reader
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Typeable
import GHC.Float (double2Float)
import qualified Graphics.UI.GLFW as GLFW

import Behaviour
import qualified GLWrap as GL

data FrameInput = FrameInput { _frameTime :: Float
                             , _frameKeys :: Keys
                             , _frameMouse :: (Float, Float)
                             , _frameScroll :: (Float, Float)
                             , _frameViewPort :: (Int, Int)
                             }

makeLenses ''FrameInput

data Renderer r s = Renderer { rendererInit :: s -> IO r
                             , rendererFun :: s -> r -> IO r
                             , rendererCleanup :: r -> IO ()
                             }

data HostConfig t = HostConfig { eventTime :: Event t Float
                               , triggerTime :: IORef (Maybe (EventTrigger t Float))
                               }

-- type GLApp t m r s = (Reflex t, MonadHold t m, MonadFix m, MonadIO m)
--                   => HostConfig t
--                   -> m (Renderer r s, Behavior t s)

type GLApp t m r s = (Reflex t, MonadHold t m, MonadFix m, MonadIO m)
                  => ReaderT (HostConfig t) m (Renderer r s, Behavior t s)

data GLFWException = GLFWException String deriving (Show, Typeable)
instance Exception GLFWException

withGLFW :: IO () -> IO ()
withGLFW a = do
  result <- GLFW.init
  case result of
    False ->
      throw $ GLFWException "GLFW failed to initialize"
    True ->
      a

host :: (forall t m. GLApp t m r s) -> IO ()
host guest = do
  withGLFW $ do
    setWindowHints
    withWindow $ \window -> do
      runSpiderHost $ do
        hostConfig <- makeHostConfig
        liftIO $ setupWindow window hostConfig
        (renderer, stateOutput) <- runHostFrame $ runReaderT guest hostConfig
        initialRenderState <- makeInitialRenderState stateOutput renderer

        foreverSt initialRenderState $ \renderState -> do
          events <- liftIO $ collectEvents hostConfig
          fireEvents events
          state <- runHostFrame $ sample stateOutput
          liftIO $ renderFrame window renderer state renderState

  where
    renderFrame :: GLFW.Window -> Renderer r s -> s -> r -> IO (Maybe r)
    renderFrame window renderer state renderState = do
      shouldClose <- GLFW.windowShouldClose window
      case shouldClose of
        True -> do
          return Nothing
        False -> do
          renderState' <- rendererFun renderer state renderState
          GLFW.swapBuffers window
          return $ Just renderState'

    timeEvents HostConfig{triggerTime} = do
      readIORef triggerTime >>= \case
        Nothing -> return []
        Just trigger -> do
          time <- double2Float . fromMaybe 0 <$> GLFW.getTime
          return [trigger :=> Identity time]

    collectEvents hostConfig = do
      GLFW.pollEvents
      return []

    setupWindow :: GLFW.Window -> HostConfig t -> IO ()
    setupWindow window hostConfig = do
      GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
      GLFW.makeContextCurrent $ Just window
      (width, height) <- GLFW.getFramebufferSize window
      GL.viewport (GL.WinCoord 0) (GL.WinCoord 0) (GL.toWidth width) (GL.toHeight height)
      GLFW.setKeyCallback window $ Just $ keyCallback hostConfig
      GLFW.setScrollCallback window $ Just $ scrollCallback hostConfig
      GLFW.setCursorPosCallback window $ Just $ cursorCallback hostConfig

    keyCallback :: HostConfig t -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
    keyCallback hostConfig window key scancode action mode = do
      return ()

    scrollCallback :: HostConfig t  -> GLFW.Window -> Double -> Double -> IO ()
    scrollCallback hostConfig window dx dy = do
      return ()

    cursorCallback :: HostConfig t -> GLFW.Window -> Double -> Double -> IO ()
    cursorCallback hostConfig window x y = do
      return ()

    makeHostConfig = do
      (e, eTriggerRef) <- newEventWithTriggerRef
      return $ HostConfig { eventTime = e
                          , triggerTime = eTriggerRef
                          }
    setWindowHints = do
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      GLFW.windowHint $ GLFW.WindowHint'Resizable False

    makeInitialRenderState stateOutput renderer = do
        initialState <- runHostFrame (sample stateOutput)
        initialRenderState <- liftIO $ rendererInit renderer initialState
        return initialRenderState

    withWindow f = do
      mayWindow <- GLFW.createWindow 800 600 "LearnOpenGL" Nothing Nothing
      case mayWindow of
        Nothing ->
          throw $ GLFWException "failed to create window"
        Just window ->
          f window `finally` GLFW.destroyWindow window


guest :: GLApp t m () ()
guest = do
  hc <- ask
  let renderer = Renderer (\() -> return ()) (\() r -> return r) (\() -> return ())
  return $ (renderer, current (constDyn ()))


foreverSt :: Monad m => a -> (a -> m (Maybe a)) -> m ()
foreverSt a f = do
  ma <- f a
  case ma of
    Just a' -> foreverSt a' f
    Nothing -> return ()
