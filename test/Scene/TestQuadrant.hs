{-------------------------------------------------------------------------------
  test/Scene/TestQuadrant.hs
  Author: Kathryn McKay

  Tests confirming the Scene.Quadrant class.
-------------------------------------------------------------------------------}
import Test.QuickCheck

import qualified Scene.Rectangle as Rectangle
import qualified Scene.Quadrant as Quadrant

instance Arbitrary Rectangle.Rectangle where
  arbitrary = do
    offsetX <- arbitrary
    offsetY <- arbitrary
    sizeX <- arbitrary
    sizeY <- arbitrary
    return (Rectangle.Rectangle [abs offsetX, abs offsetY] [abs sizeX + 1.0,
      abs sizeY + 1.0])

-- Checks if center of first quadrant produces first quadrant
prop_PartitFirst :: Rectangle.Rectangle -> Bool
prop_PartitFirst r = Quadrant.partit pt r == Quadrant.I
  where size = [ x/4 | x <- (Rectangle.size r)]
        offset = Rectangle.offset r
        pt = [offset !! 0 + size !! 0 * 3, offset !! 1 + size !! 1]

-- Checks if center of second quadrant produces second quadrant
prop_PartitSecond :: Rectangle.Rectangle -> Bool
prop_PartitSecond r = Quadrant.partit pt r == Quadrant.II
  where size = [ x/4 | x <- (Rectangle.size r)]
        offset = Rectangle.offset r
        pt = [offset !! 0 + size !! 0, offset !! 1 + size !! 1]

-- Checks if center of third quadrant produces third quadrant
prop_PartitThird :: Rectangle.Rectangle -> Bool
prop_PartitThird r = Quadrant.partit pt r == Quadrant.III
  where size = [ x/4 | x <- (Rectangle.size r)]
        offset = Rectangle.offset r
        pt = [offset !! 0 + size !! 0, offset !! 1 + size !! 1 * 3]

-- Checks if center of fourth quadrant produces fourth quadrant
prop_PartitFourth :: Rectangle.Rectangle -> Bool
prop_PartitFourth r = Quadrant.partit pt r == Quadrant.IV
  where size = [ x/4 | x <- (Rectangle.size r)]
        offset = Rectangle.offset r
        pt = [offset !! 0 + size !! 0 * 3, offset !! 1 + size !! 1 * 3]

-- Checks if point in center produces root quadrant
prop_PartitCenter :: Rectangle.Rectangle -> Bool
prop_PartitCenter r = Quadrant.partit pt r == Quadrant.Root
  where pt = Rectangle.center r

-- Checks if point on x axis produces root quadrant
prop_PartitXLined :: Rectangle.Rectangle -> Bool
prop_PartitXLined r = Quadrant.partit pt r == Quadrant.Root
  where size = [ x/4 | x <- (Rectangle.size r)]
        offset = Rectangle.offset r
        center = Rectangle.center r
        pt = [center !! 0, offset !! 1 + size !! 1]


-- Checks if point on y axis produces root quadrant
prop_PartitYLined :: Rectangle.Rectangle -> Bool
prop_PartitYLined r = Quadrant.partit pt r == Quadrant.Root
  where size = [ x/4 | x <- (Rectangle.size r)]
        offset = Rectangle.offset r
        center = Rectangle.center r
        pt = [offset !! 0 + size !! 0, center !! 1]

-- Checks that sliced quad has appropriate size
checkSliceSize :: Quadrant.Quadrant -> Rectangle.Rectangle -> Bool
checkSliceSize quad rect = pred 0 && pred 1
  where parentSize = Rectangle.size rect
        sliceSize = Rectangle.size (Quadrant.slice quad rect)
        pred k = sliceSize !! k == parentSize !! k / 2

-- Checks that all sizes returned by slice are legit
prop_SliceSize rect = (all (\x -> checkSliceSize x rect)
    [Quadrant.I .. Quadrant.IV])
  && (Rectangle.size (Quadrant.slice Quadrant.Root rect))
    == (Rectangle.size rect)

-- Checks that offsets returned by slices are legit
checkSliceOffset :: Quadrant.Quadrant -> [Bool] -> Rectangle.Rectangle -> Bool
checkSliceOffset quad bOutCoords rect = Rectangle.offset slice == ex
  where slice = Quadrant.slice quad rect
        offset = Rectangle.offset rect
        center = Rectangle.center rect
        offset' n = if bOutCoords !! n then center !! n else offset !! n
        ex = [offset' 0, offset' 1]

-- Checks that all offsets returned by slices are legit
prop_SliceOffset :: Rectangle.Rectangle -> Bool
prop_SliceOffset rect = all check [(Quadrant.I, True, False)
  , (Quadrant.II, False, False)
  , (Quadrant.III, False, True)
  , (Quadrant.IV, True, True)
  , (Quadrant.Root, False, False)]
  where check (quad, bOutX, bOutY) = checkSliceOffset quad [bOutX, bOutY] rect

main :: IO ()
main = do mapM_ quickCheck [prop_PartitFirst, prop_PartitSecond
                            , prop_PartitThird, prop_PartitFourth
                            , prop_PartitCenter, prop_PartitXLined
                            , prop_PartitYLined
                            , prop_SliceSize
                            , prop_SliceOffset]
