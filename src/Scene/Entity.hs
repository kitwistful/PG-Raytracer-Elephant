{-------------------------------------------------------------------------------
  Scene/Entity.hs
  Author: Kathryn McKay

  Describes an OnGrid data type with a name and position.
-------------------------------------------------------------------------------}
module Scene.Entity (
  Entity(..)
) where

import Scene.OnGrid

data Entity = Entity String [Double]
  deriving (Show, Eq)

instance OnGrid Entity where
  coord (Entity _ position) = position
