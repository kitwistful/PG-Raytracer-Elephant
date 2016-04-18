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
  deriving (Eq)

instance Show Entity where
  show (Entity name pt) = "(" ++ name ++ ": " ++ show (pt !! 0) ++ ", " ++ show (pt !! 1) ++ ")"

instance OnGrid Entity where
  coord (Entity _ position) = position
