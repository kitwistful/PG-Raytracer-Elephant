{-------------------------------------------------------------------------------
  test/Scene/TestRectangle.hs
  Author: Kathryn McKay

  Tests confirming the Scene.Rectangle class.
-------------------------------------------------------------------------------}
import Test.QuickCheck

import qualified Scene.Rectangle as Rectangle

instance Arbitrary Rectangle.Rectangle where
  arbitrary = do
    offsetX <- arbitrary
    offsetY <- arbitrary
    sizeX <- arbitrary
    sizeY <- arbitrary
    return (Rectangle.Rectangle [abs offsetX, abs offsetY] [abs sizeX + 1.0,
      abs sizeY + 1.0])

main :: IO ()
main = do
  -- Test that contains returns true for the offset value, iff the size is
  -- greater than zero.
  quickCheck ((\x -> (Rectangle.contains x (Rectangle.offset x)) ==
    (all (\y -> y > 0) (Rectangle.size x)))
    :: Rectangle.Rectangle -> Bool)

  -- Test that contains returns false for offset + size.
  quickCheck ((\x -> (Rectangle.contains x [(fst pt) + (snd pt) | pt <- (zip (Rectangle.offset x) (Rectangle.size x))]) == False)
    :: Rectangle.Rectangle -> Bool)

  -- Test that contains returns true for a point in the center of the
  -- plane.
  quickCheck ((\x -> (Rectangle.contains x [ (fst pt + (snd pt)/2) |  pt <- (zip (Rectangle.offset x) (Rectangle.size x))]) ==
    (all (\y -> y/2 > 0) (Rectangle.size x)))
    :: Rectangle.Rectangle -> Bool)

  -- Test that getCenter is contained within the plane.
  quickCheck ((\x -> (Rectangle.contains x (Rectangle.center x)) == True)
    :: Rectangle.Rectangle -> Bool)
