{-------------------------------------------------------------------------------
  test/Scene/TestQuadTree.hs
  Author: Kathryn McKay

  Tests confirming the Scene.QuadTree class.
-------------------------------------------------------------------------------}
import Test.QuickCheck

import qualified Scene.QuadTree as QuadTree
import qualified Scene.Rectangle as Rectangle
import Scene.Entity

instance Arbitrary QuadTree.QuadTree where
  arbitrary = do
    return (QuadTree.create (Rectangle.Rectangle [-1.0, -1.0] [2.0, 2.0]))

checkMass :: IO ()
checkMass =
  do mapM_
      check [
      -- Test with empty tree
        ([], 0)

      -- Test with 1 insertion
      , ([Entity "b" [0.0, 0.0]], 1)

      -- Test with 5 insertions along boundaries
      , ([(Entity "a" [0.0, 0.0]), (Entity "b" [-0.5, 0.0]), (Entity "c" [0.5, 0.0]), (Entity "d" [0.0, 0.5]), (Entity "e" [0.0, -0.5])], 5)

      -- Test with 5 insertions to cause 1 split
      , ([(Entity "a" [0.5, 0.5]), (Entity "b" [-0.5, 0.0]), (Entity "c" [0.5, 0.0]), (Entity "d" [0.0, 0.5]), (Entity "e" [0.0, -0.5])], 5)]
  where check (o, m) = do quickCheck ((\t -> QuadTree.mass (QuadTree.insert o t) == m) :: QuadTree.QuadTree -> Bool)

checkInsertHeight :: IO ()
checkInsertHeight =
  do mapM_
      check [
      -- Test with empty tree
        ([], 1)

      -- Test with 1 insertion
      , ([Entity "b" [0.0, 0.0]], 1)

      -- Test with 5 insertions along boundaries
      , ([(Entity "a" [0.0, 0.0]), (Entity "b" [-0.5, 0.0]), (Entity "c" [0.5, 0.0]), (Entity "d" [0.0, 0.5]), (Entity "e" [0.0, -0.5])], 1)

      -- Test with 5 insertions to cause 1 split
      , ([(Entity "a" [0.5, 0.5]), (Entity "b" [-0.5, 0.0]), (Entity "c" [0.5, 0.0]), (Entity "d" [0.0, 0.5]), (Entity "e" [0.0, -0.5])], 2)]
  where check (o, h) = do quickCheck ((\t -> QuadTree.height (QuadTree.insert o t) == h) :: QuadTree.QuadTree -> Bool)

main :: IO ()
main = do
  putStrLn "[ Start Testing Height ]"
  checkInsertHeight
  putStrLn "[ Finish Testing Height ]"
  putStrLn "[ Start Testing Mass ]"
  checkMass
  putStrLn "[ Finish Testing Mass ]"
