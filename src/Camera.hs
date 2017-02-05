{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Camera
  ( Camera
  , camPos
  , camFront
  , camUp
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

data Camera = Camera { _camPos :: V3 Float
                     , _camFront :: V3 Float
                     , _camUp :: V3 Float
                     } deriving (Eq, Show)

makeLenses ''Camera

{-| Moves camera along its front vector.

>>> :{
      let cam = def & camPos .~ (V3 0 0 0)
                    & camFront .~ (V3 0 0 1)
                    & camUp .~ (V3 0 1 0)
>>> :}

>>> (camAdvance 0.5 cam) ^. camPos
V3 0.0 0.0 0.5
-}
camAdvance :: Float -> Camera -> Camera
camAdvance delta camera = camera { _camPos = _camPos camera + pure delta * _camFront camera }

{-| Move camera perpendicualar to its front and up vectors (i.e. strafe).
Negative delta move to the left.

>>> :{
      let cam = def & camPos .~ (V3 0 0 0)
                    & camFront .~ (V3 0 0 1)
                    & camUp .~ (V3 0 1 0)
>>> :}

>>> (camStrafe 0.5 cam) ^. camPos
V3 (-0.5) 0.0 0.0
-}
camStrafe :: Float -> Camera -> Camera
camStrafe delta camera = camera { _camPos = _camPos camera + pure delta * strafeDirection }
  where strafeDirection = normalize (cross (_camFront camera) (_camUp camera))

viewMatrix :: Camera -> M44 Float
viewMatrix Camera{..} = lookAt _camPos (_camPos + _camFront) _camUp

instance Default Camera where
  def = Camera { _camPos = V3 0 0 3
               , _camFront = V3 0 0 (-1)
               , _camUp = V3 0 1 0
               }
