{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module POV where

import Control.Lens
import Control.Monad.State
import Data.Default
import Linear.Matrix
import Linear.V3

import Camera
import Utils
import qualified GLWrap as GL

data POV = POV { _povCamera :: Camera
               , _povSpeed :: Float
               , _povPanSensitivity :: Float
               , _povFov :: Float
               , _povFovSensitivity :: Float
               }

instance Default POV where
  def = POV { _povCamera = def
            , _povSpeed = 0.5
            , _povPanSensitivity = 0.15
            , _povFov = 45
            , _povFovSensitivity = 15
            }

data Movement = MoveForward | MoveBackward
data Strafe = StrafeLeft | StrafeRight
data ChangeHeight = LiftUp | LiftDown
makeLenses ''POV

data Advance = Advance { _advMovement :: Maybe Movement
                       , _advStrafe :: Maybe Strafe
                       , _advHeight :: Maybe ChangeHeight
                       , _advPan :: Maybe (Float, Float)
                       , _advFov :: Maybe Float
                       }

makeLenses ''Advance

advancePov :: Advance -> Float -> POV -> POV
advancePov Advance{..} timeDelta pov = foldl (&) pov [maybeMove, maybeStrafe, maybePan, maybeFov, maybeHeight]
  where
    maybeMove :: POV -> POV
    maybeMove pov = case _advMovement of
      Nothing -> pov
      Just movement -> pov & povCamera %~ camAdvance (movementAmount movement)

    maybeStrafe :: POV -> POV
    maybeStrafe pov = case _advStrafe of
      Nothing -> pov
      Just strafe -> pov & povCamera %~ camStrafe (strafeAmount strafe)

    maybePan :: POV -> POV
    maybePan pov = case _advPan of
      Nothing -> pov
      Just (yaw, pitch) ->
        let yaw' = yaw * pov^.povPanSensitivity * timeDelta
            pitch' = negate $ pitch * pov^.povPanSensitivity * timeDelta
        in
          pov & povCamera %~ panCamera (yaw', pitch')

    maybeFov :: POV -> POV
    maybeFov pov = case _advFov of
      Nothing -> pov
      Just fov -> pov & povFov %~ (\f -> clamp 35 170 $ f + fov)

    maybeHeight :: POV -> POV
    maybeHeight pov = case _advHeight of
      Nothing -> pov
      Just m -> pov & povCamera . camPos . _y  %~ (+ verticalMovementAmount m)

    verticalMovementAmount m = verticalMovementSign m * timeDelta * pov^.povSpeed
    verticalMovementSign LiftUp = 1
    verticalMovementSign LiftDown = -1

    movementAmount m = movementSign m * timeDelta *  pov^.povSpeed
    movementSign MoveBackward = -1
    movementSign MoveForward = 1

    strafeAmount m = strafeSign m * timeDelta * pov^.povSpeed
    strafeSign StrafeLeft = -1
    strafeSign StrafeRight = 1

povViewMat :: POV -> M44 Float
povViewMat pov =
  viewMatrix $ pov^.povCamera

povProjectionMat :: GL.Width -> GL.Height -> POV -> M44 Float
povProjectionMat width height pov =
  GL.perspectiveMatrix (GL.Deg $ pov^.povFov) (screenRatio width height) 0.1 100
  where
    screenRatio (GL.Width w) (GL.Height h) = (fromIntegral w / fromIntegral h)
