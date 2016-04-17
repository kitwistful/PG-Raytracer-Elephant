{-------------------------------------------------------------------------------
  Scene/OnGrid.hs
  Author: Kathryn McKay

  Typeclass specifying an object that has an (x,y) position that can be
  evaluated.
-------------------------------------------------------------------------------}
-- | This module has the positionable typeclass in it.
module Scene.OnGrid (
  OnGrid(..)
) where

-- | Can be expressed as existing On a cartesian Grid.
class OnGrid a where
  -- | Retrieves the (x, y) position of the instance.
  coord :: a -> [Double]
