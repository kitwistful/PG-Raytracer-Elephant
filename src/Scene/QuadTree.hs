{-------------------------------------------------------------------------------
  Scene/QuadTree.hs
  Author: Kathryn McKay

  Storage and partitioning of OnGrid types based on their positions.
  Split happens after exceeding 3 objects in a node, though there is no limit
  on the number of nodes which accumulate on the node boundaries (objects which
  lie on the center line are not kicked to lower nodes)
-------------------------------------------------------------------------------}
module Scene.QuadTree (
  QuadTree(..)
, splitThreshold
, create
, height
, insert
, mass
) where

import qualified Scene.Rectangle as Rectangle
import Scene.Entity
import Scene.OnGrid

-- | Space partitioning structure.
data QuadTree = Bud | Node { areaCovered :: Rectangle.Rectangle
                             , objects :: [[Entity]], nodes:: [QuadTree]
                             , depth :: Int}

-- | Demonstrates tree structure and contents.
instance Show QuadTree where
  show Bud = ""
  show Node {areaCovered=areaCovered, objects=objects, nodes=nodes, depth=depth}
    = "\n" ++ take depth (repeat '│') ++ "├─" ++ show objects ++ show nodes

-- | This is the number of nodes allowed in a node before it splits
splitThreshold = 3

-- | Initializes an empty paritioning space with specified size & offset
create :: Rectangle.Rectangle -> QuadTree
create rect = Node { areaCovered = rect, objects = [],
  nodes = [Bud, Bud, Bud, Bud], depth = 0 }

-- | Retrieves maximum depth below this node.
height :: QuadTree -> Int
height tree = case tree of Bud -> 0
                           tree -> 1 + maximum (map height (nodes tree))

-- | Returns a version of the tree with the objects partitioned inside it
insert :: [Entity] -> QuadTree -> QuadTree
insert objects tree
  | length (objects) == 1 = tree -- TODO: implement after quadrant functions work.
  | length (objects) == 0 = tree
  | otherwise = insert (tail objects) (insert [head objects] tree)

-- | Returns number of objects in tree.
mass :: QuadTree -> Int
mass tree = case tree of Bud -> 0
                         Node _ objects nodes _ -> (length objects) + sum (map mass nodes)
