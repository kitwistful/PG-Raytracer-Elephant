{-------------------------------------------------------------------------------
  Scene/Rectangle.hs
  Author: Kathryn McKay
-------------------------------------------------------------------------------}
-- | Functions to manipulate a geometric rectangle.
module Scene.Rectangle (
  Rectangle(..)
, center
, contains
) where

-- | Defines an area with an offset and a size.
data Rectangle = Rectangle { offset :: [Double], size :: [Double] }
  deriving (Eq)

instance Show Rectangle where
  show (Rectangle offset size) = "(From: " ++ show (offset !! 0) ++ ", " ++ show (offset !! 1) ++ " | To: " ++ show (farCorner !! 0) ++ ", " ++ show (farCorner !! 1) ++ ")"
    where farCorner = map (\(x,y) -> x + y) (zip offset size)
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
