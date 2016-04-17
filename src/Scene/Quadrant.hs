{-------------------------------------------------------------------------------
  Scene/Quadrant.hs
  Author: Kathryn McKay

  Defines a rectangle as being able to be split into four quadrants.
-------------------------------------------------------------------------------}
module Scene.Quadrant (
  Quadrant(..)
, partit
, slice
) where

import qualified Scene.Rectangle as Rectangle

data Quadrant = I | II | III | IV | Root
  deriving (Show, Enum, Eq)

-- | outputs the quadrant of rectangle which the point falls on.
partit :: [Double] -> Rectangle.Rectangle -> Quadrant
partit point rectangle
  | x == xBound || y == yBound = Root
  | y < yBound && x > xBound = I
  | y < yBound && x < xBound = II
  | y > yBound && x > xBound = IV
  | y > yBound && x < xBound = III
  where x = point !! 0
        y = point !! 1
        center = Rectangle.center rectangle
        xBound = center !! 0
        yBound = center !! 1

slice :: Quadrant -> Rectangle.Rectangle -> Rectangle.Rectangle
slice quad rect = case quad of
  I -> Rectangle.Rectangle [offset !! 0 + size !! 0, offset !! 1] size
  II -> Rectangle.Rectangle [offset !! 0, offset !! 1] size
  III -> Rectangle.Rectangle [offset !! 0 , offset !! 1 + size !! 1] size
  IV -> Rectangle.Rectangle [offset !! 0 + size !! 0, offset !! 1 + size !! 1] size
  Root -> rect
  where size = [ x/2 | x <- Rectangle.size rect ]
        offset = Rectangle.offset rect
