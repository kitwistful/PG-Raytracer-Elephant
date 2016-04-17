{-------------------------------------------------------------------------------
  Scene/Rectangle.hs
  Author: Kathryn McKay

  Defines an area with an offset and a size.
-------------------------------------------------------------------------------}
module Scene.Rectangle (
  Rectangle(..)
, center
, contains
) where

data Rectangle = Rectangle { offset :: [Double], size :: [Double] }
  deriving (Show, Eq)

-- | Returns the point that lies in the center of the plane.
center :: Rectangle -> [Double]
center rectangle = [((fst x)*2 + (snd x))/2
  | x <- (zip (offset rectangle) (size rectangle))]

-- | True, when the point given is within the rectangle's range, with
-- | inclusivity like [) so that the top left edge is included while the
-- | bottom right edge is not.
contains :: Rectangle -> [Double] -> Bool
contains rectangle point = all pred (zip3 point (offset rectangle)
  (size rectangle))
  where pred (x, y, z) = x >= y && abs x < y + z
