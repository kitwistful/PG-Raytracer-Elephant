{-|
  QuadTree module
  version 2
  Author: Kathryn McKay
  
  Exports this space partitioning type for 2D space.
-}
module QuadTree (
  QuadTree(..)
, quadTreeHeight
) where

-- | Space partitioning structure.
data QuadTree = QuadTreeTip
  | QuadTreeNode { size :: (Double, Double)
                 , objects :: [(Double, Double)]
                 , children :: (QuadTree, QuadTree, QuadTree, QuadTree)
                 , depth :: Int}
  
-- | Converts 4 tuple to list.
listifyChildren (x1, x2, x3, x4) = [x1, x2, x3, x4]

-- | Retrieves maximum depth below this node.
quadTreeHeight :: QuadTree -> Int
quadTreeHeight tree = case tree of QuadTreeTip -> 0
                                   tree -> 1 + maximum (map quadTreeHeight
                                          (listifyChildren(children tree)))

-- | Demonstrates tree structure and contents.
instance Show QuadTree where
  show QuadTreeTip = ""
  show QuadTreeNode {size=size, objects=objects, children=children, depth=depth}
    = "\n" ++ take depth (repeat '│') ++ "├─" ++ show objects
    ++ concat (map show (listifyChildren children))
