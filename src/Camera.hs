{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera
  ( Camera
  , camPos
  , camFront
  , camUp
  , camYaw
  , camPitch
  , camAdvance
  , camStrafe
  , viewMatrix
  ) where

import Linear.V3
import Data.Default
import Control.Lens
import Linear.Matrix
import Linear.Projection (lookAt)
import Linear.Metric (normalize)
import Debug.Trace

import Utils

data Camera = Camera { _camPos :: V3 Float
                     , _camUp :: V3 Float
                     , _camYaw :: Float
                     , _camPitch :: Float
                     } deriving (Eq, Show)

camPos :: Lens' Camera (V3 Float)
camPos = lens _camPos $ \c p -> c { _camPos = p }

camUp :: Lens' Camera (V3 Float)
camUp = lens _camUp $ \c u -> c { _camUp = u }

camYaw :: Lens' Camera Float
camYaw = lens _camYaw $ \c y -> c { _camYaw = y }

clampPitch :: Float -> Float
clampPitch = clamp (-89) 89

camPitch :: Lens' Camera Float
camPitch = lens _camPitch $ \c p -> c { _camPitch = clampPitch p }


camFront :: Camera -> V3 Float
camFront Camera{..} = normalize $ V3 (cos _camYaw * cos _camPitch) (sin _camPitch) (sin _camYaw * cos _camPitch)

{-| Moves camera along its front vector.

>>> :{
      let cam = def & camPos .~ (V3 0 0 0)
                    & camYaw .~ (pi / 2)
                    & camUp .~ (V3 0 1 0)
>>> :}

>>> (camAdvance 0.5 cam) ^. camPos
V3 0.0 0.0 0.5
-}
camAdvance :: Float -> Camera -> Camera
camAdvance delta camera = camera { _camPos = _camPos camera + pure delta * camFront camera }

{-| Move camera perpendicualar to its front and up vectors (i.e. strafe).
Negative delta move to the left.

>>> :{
      let cam = def & camPos .~ (V3 0 0 0)
                    & camYaw .~ (pi / 2)
                    & camUp .~ (V3 0 1 0)
>>> :}

>>> (camStrafe 0.5 cam) ^. camPos
V3 (-0.5) 0.0 0.0
-}
camStrafe :: Float -> Camera -> Camera
camStrafe delta camera = camera { _camPos = _camPos camera + pure delta * strafeDirection }
  where strafeDirection = normalize (cross (camFront camera) (_camUp camera))

viewMatrix :: Camera -> M44 Float
viewMatrix (cam@Camera{..}) = lookAt _camPos (_camPos + camFront cam) _camUp

instance Default Camera where
  def = Camera { _camPos = V3 0 0 3
               , _camUp = V3 0 1 0
               , _camPitch = 0
               , _camYaw = 0
               }
