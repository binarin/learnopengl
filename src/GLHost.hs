{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module GLHost
  ( Renderer(..)
  , HostConfig(eventTime, cursorDyn, scrollEvent)
  , keyEvent
  , GLMonad
  , GLApp
  , host
  ) where

import Reflex
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents, EventTrigger)
import Control.Lens
import Control.Monad.Identity (Identity(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (readIORef, IORef)
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Typeable
import GHC.Float (double2Float)
import qualified Graphics.UI.GLFW as GLFW
import Data.Default
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Control.Concurrent.STM as STM

import Camera
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
                               , eventQueue :: TQueue.TQueue InputEvent
                               , keyEvents :: M.Map GLFW.Key (Event t Bool, IORef (Maybe (EventTrigger t Bool)))
                               , cursorDyn :: Dynamic t (Float, Float)
                               , cursorTrigger :: IORef (Maybe (EventTrigger t (Float, Float)))
                               , scrollEvent :: Event t (Float, Float)
                               , scrollTrigger :: IORef (Maybe (EventTrigger t (Float, Float)))
                               }

-- type GLApp t m r s = (Reflex t, MonadHold t m, MonadFix m, MonadIO m)
--                   => HostConfig t
--                   -> m (Renderer r s, Behavior t s)

type GLMonad t m a = (Reflex t, MonadHold t m, MonadFix m, MonadIO m)
                   => ReaderT (HostConfig t) m a

keyEvent :: GLFW.Key -> GLMonad t m (Event t Bool)
keyEvent k = do
  me <- M.lookup k <$> asks keyEvents
  case me of
    Just (e, _) ->
      return e
    Nothing ->
      return $ never

type GLApp t m r s = GLMonad t m (Renderer r s, Behavior t s)

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
        liftIO $ GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
        hostConfig <- mkHostConfig window
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

    drainQueue (hc@HostConfig{eventQueue}) = do
      maybeEvent <- STM.atomically $ TQueue.tryReadTQueue eventQueue
      case maybeEvent of
        Nothing ->
          return []
        Just event -> do
          rest <- drainQueue hc
          wannaTrigger <- triggerableEvent hc event
          case wannaTrigger of
            Nothing ->
              return rest
            Just fire ->
              return $ fire:rest

    triggerableEvent hc (KeyEvent key _ GLFW.KeyState'Pressed _) =
      case M.lookup key (keyEvents hc) of
        Nothing ->
          return Nothing
        Just (_, tRef) ->
          fmap (:=> Identity True) <$> liftIO (readIORef tRef)
    triggerableEvent hc (KeyEvent key _ GLFW.KeyState'Released _) =
      case M.lookup key (keyEvents hc) of
        Nothing ->
          return Nothing
        Just (_, tRef) ->
          fmap (:=> Identity False) <$> liftIO (readIORef tRef)
    triggerableEvent hc (MouseEvent x y) =
      fmap (:=> Identity (x, y)) <$> liftIO (readIORef $ cursorTrigger hc)
    triggerableEvent hc (ScrollEvent x y) =
      fmap (:=> Identity (x, y)) <$> liftIO (readIORef $ scrollTrigger hc)
    triggerableEvent _ _ = return Nothing

    collectEvents hostConfig = do
      GLFW.pollEvents
      timeE <- timeEvents hostConfig
      inputEvents <- drainQueue hostConfig
      return $ timeE ++ inputEvents

    setupWindow :: GLFW.Window -> HostConfig t -> IO ()
    setupWindow window hostConfig = do
      GLFW.makeContextCurrent $ Just window
      (width, height) <- GLFW.getFramebufferSize window
      GL.viewport (GL.WinCoord 0) (GL.WinCoord 0) (GL.toWidth width) (GL.toHeight height)
      GLFW.setKeyCallback window $ Just $ keyCallback hostConfig
      GLFW.setScrollCallback window $ Just $ scrollCallback hostConfig
      GLFW.setCursorPosCallback window $ Just $ cursorCallback hostConfig

    keyCallback :: HostConfig t -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
    keyCallback hostConfig window key scancode action mode =
      STM.atomically $ TQueue.writeTQueue (eventQueue hostConfig) $ KeyEvent key scancode action mode

    scrollCallback :: HostConfig t  -> GLFW.Window -> Double -> Double -> IO ()
    scrollCallback hostConfig window dx dy = do
      STM.atomically $ TQueue.writeTQueue (eventQueue hostConfig) $ ScrollEvent (double2Float dx) (double2Float dy)

    cursorCallback :: HostConfig t -> GLFW.Window -> Double -> Double -> IO ()
    cursorCallback hostConfig window x y = do
      STM.atomically $ TQueue.writeTQueue (eventQueue hostConfig) $ MouseEvent (double2Float x) (double2Float y)

    mkHostConfig window = do
      (e, eTriggerRef) <- newEventWithTriggerRef
      eventQueue <- liftIO $ TQueue.newTQueueIO
      let allKeys = [GLFW.Key'Unknown .. GLFW.Key'Menu]
      keyEvents <- M.fromList <$> flip mapM allKeys (\k -> do
                                                        et <- newEventWithTriggerRef
                                                        return $ (k, et))
      (scrollEvent, scrollTrigger) <- newEventWithTriggerRef
      (cursorEvent, cursorTrigger) <- newEventWithTriggerRef
      (cx, cy) <- liftIO $ GLFW.getCursorPos window
      cursorDyn <- holdDyn (double2Float cx, double2Float cy) cursorEvent

      return $ HostConfig { eventTime = e
                          , triggerTime = eTriggerRef
                          , eventQueue = eventQueue
                          , keyEvents = keyEvents
                          , scrollEvent = scrollEvent
                          , scrollTrigger = scrollTrigger
                          , cursorDyn = cursorDyn
                          , cursorTrigger = cursorTrigger
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

camera :: GLMonad t m (Dynamic t Camera)
camera = do -- Reader monad
  et :: Event t Float <- asks eventTime
  -- dyn <- lift $ foldDyn (\_ _ -> def) def et
  foldDyn (\_ _ -> def) def et
  -- return dyn

guest :: GLApp t m () ()
guest = do
  hc <- ask
  let renderer = Renderer (\() -> return ()) (\() r -> return r) (\() -> return ())
  et <- asks eventTime
  c <- camera
  state <- foldDyn (\_ _ -> ()) () et
  return $ (renderer, current state)

foreverSt :: Monad m => a -> (a -> m (Maybe a)) -> m ()
foreverSt a f = do
  ma <- f a
  case ma of
    Just a' -> foreverSt a' f
    Nothing -> return ()
