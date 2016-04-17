{-------------------------------------------------------------------------------
  test/Scene/TestEntity.hs
  Author: Kathryn McKay

  Tests confirming the Scene.Entity class.
-------------------------------------------------------------------------------}
import Test.QuickCheck

import Scene.Entity
import Scene.OnGrid

instance Arbitrary Entity where
  arbitrary = do
    name <- arbitrary
    x <- arbitrary
    y <- arbitrary
    return (Entity name [x, y])

main :: IO ()
main = do
  -- Test that value returned by coord is set.
  quickCheck ((\x -> length (coord x) == 2) :: Entity -> Bool)
