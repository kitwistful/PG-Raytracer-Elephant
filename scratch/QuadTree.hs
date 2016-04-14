module QuadTree (
  QuadTree(..)
) where

data QuadTree = QuadTreeEmpty Int
  | QuadTreeLeaf (Float, Float) [(Float, Float)] Int
  | QuadTreeSplitNode (QuadTree) (QuadTree) (QuadTree) (QuadTree) Int
  
instance Show QuadTree where
  show (QuadTreeSplitNode x1 x2 x3 x4 level) = (take level (repeat '│')) ++ "├─\n" ++ show x1 ++ " " ++ show x2 ++ " "
    ++ show x3 ++ " " ++ show x4
  show (QuadTreeLeaf _ _ level) = (take level (repeat '│')) ++ "├─leaf\n"
  show (QuadTreeEmpty level) = (take level (repeat '│')) ++ "├─empty\n"