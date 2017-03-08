{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LightingReflex where

import Control.Monad.Reader
import Reflex
import Data.Default
import Linear.V3
import Linear.V3
import Linear.Matrix hiding (trace)
import Control.Lens

import GLHost
import qualified Data.Map as M
import qualified GLWrap as GL
import qualified Graphics.UI.GLFW as GLFW
import POV
import Camera
import Lighting (WhiteCube, mkWhiteCube, renderWhiteCube, MatrixStack(..))

data RenderState = RenderState { _whiteCube :: WhiteCube
                               }
makeLenses ''RenderState

data GameState = GameState { _stPov :: POV
                           , _delta :: Float
                           , _width :: GL.Width
                           , _height :: GL.Height
                           , _lolo :: Bool
                           }
makeLenses ''GameState

instance Default GameState where
  def = GameState pov delta width height False
    where pov = def & povCamera . camPos .~ (V3 0 0 10)
                    & povCamera . camYaw .~ (-pi/2)
          delta = 0
          width = GL.Width 800
          height = GL.Height 600

renderer :: Renderer RenderState GameState
renderer = Renderer initR renderR cleanupR

initR _ = do
  ls <- mkWhiteCube
  return $ RenderState { _whiteCube = ls }

renderR gs rs = do
  GL.clearColor $ GL.RGBA 0.2 0.3 0.3 1.0
  GL.clear [GL.ClearColor, GL.ClearDepth]
  putStrLn $ show (gs^.stPov.povCamera.camPitch)

  let viewMat = povViewMat $ gs^.stPov
  let projectionMat = povProjectionMat (gs^.width) (gs^.height) (gs^.stPov)
  let lightModelMat = identity

  renderWhiteCube (MatrixStack lightModelMat viewMat projectionMat) (rs^.whiteCube)

  return rs

cleanupR _ = do
  return ()

data CameraConfig = CameraConfig

tickEvent :: GLMonad t m (Dynamic t Float)
tickEvent = do
  et <- asks eventTime
  delta <- fmap snd <$> foldDyn (\abs (prev, delta) -> (abs, abs -prev)) (0, 0) et
  return delta

keyToggle :: a -> [(GLFW.Key, a)] -> GLMonad t m (Dynamic t a)
keyToggle noKeyValue bindings = do
  onEvents <- mapM (\(key, val) -> do
                       ev <- keyEvent key
                       return $ const val <$> ffilter id ev
                   ) bindings
  allKeyEvents <- mapM (keyEvent . fst) bindings
  let offEvent = const noKeyValue <$> ffilter not (leftmost allKeyEvents)
      targetEvent = leftmost $ onEvents ++ [offEvent]
  lift $ holdDyn noKeyValue targetEvent


camDyn :: forall t m. CameraConfig -> GLMonad t m (Dynamic t POV)
camDyn conf = do
  tick <- tickEvent
  move <- keyToggle Nothing [(GLFW.Key'W, Just MoveForward)
                            ,(GLFW.Key'S, Just MoveBackward)
                            ]
  strafe <- keyToggle Nothing [(GLFW.Key'A, Just StrafeLeft)
                              ,(GLFW.Key'D, Just StrafeRight)
                              ]
  cursor <- asks cursorDyn
  scroll <- asks scrollEvent
  (ix, iy) <- lift $ sample $ current cursor
  cursorDeltaDyn <- fmap fst <$> foldDyn (\(cx, cy) ((dx, dy), (px, py)) -> ((cx - px, cy - py), (cx, cy))) ((0, 0), (ix, iy)) (updated cursor)
  let cursorDeltaForTick = leftmost [Just <$> updated cursorDeltaDyn, const Nothing <$> updated tick]
  pan <- lift $ holdDyn Nothing cursorDeltaForTick
  fov <- lift $ holdDyn Nothing $ leftmost [Just . snd <$> scroll, const Nothing <$> updated tick]

  let a :: Dynamic t Advance = Advance <$> move <*> strafe <*> pan <*> fov
      ea = (,) <$> a <*> tick

  lift $ foldDyn (\(adv, dt) pov -> advancePov adv dt pov) def (updated ea)

guest :: GLApp t m RenderState GameState
guest = do
  et <- asks eventTime
  aP <- keyEvent GLFW.Key'A
  delta <- fmap snd <$> foldDyn (\abs (prev, delta) -> (abs, abs - prev)) (0, 0) et
  apd <- lift $ holdDyn False aP
  cam <- camDyn CameraConfig
  let st = GameState <$> cam <*> constDyn 0 <*> constDyn (GL.Width 800) <*> constDyn (GL.Height 600) <*> apd
  return (renderer, current st)

go :: IO ()
go = host guest
