{-------------------------------------------------------------------------------
  Scene/QuadTree.hs
  Author: Kathryn McKay

  Storage and partitioning of OnGrid types based on their positions.
  Split happens after exceeding 3 objects in a node.
-------------------------------------------------------------------------------}
module Scene.QuadTree (
  QuadTree(..)
, splitThreshold
, create
, height
, insert
, mass
, partit
) where

import qualified Scene.Rectangle as Rectangle
import qualified Scene.Quadrant as Quadrant
import Scene.Entity
import Scene.OnGrid

-- | Space partitioning structure.
data QuadTree = Bud | Node { areaCovered :: Rectangle.Rectangle
                             , objects :: [Entity], nodes:: [QuadTree]
                             , depth :: Int}
  deriving (Eq)

-- | Demonstrates tree structure and contents.
instance Show QuadTree where
  show Bud = ""
  show Node {areaCovered=areaCovered, objects=objects, nodes=nodes, depth=depth}
    = take depth (repeat '│') ++ show areaCovered ++ "\n" ++ "├─" ++ show objects ++ concat (map (\x -> if (x == Bud) then "" else ("\n" ++ show x)) nodes)

-- | This is the number of nodes allowed in a node before it splits
splitThreshold = 3

-- | adds objects to the object list.
append :: [Entity] -> QuadTree -> QuadTree
append objs (Node a o n d) = Node a (o ++ objs) n d

-- | Returns tree corresponding to quadrant.
subnode :: Quadrant.Quadrant -> QuadTree -> QuadTree
subnode quad (Bud) = Bud
subnode quad (Node a o nodes d) = case quad of
  Quadrant.I -> nodes !! 0
  Quadrant.II -> nodes !! 1
  Quadrant.III -> nodes !! 2
  Quadrant.IV ->  nodes !! 3
  Quadrant.Root -> (Node a o nodes d)

-- | Creates a new branch with object inside
bloom :: Entity -> Quadrant.Quadrant -> QuadTree -> QuadTree
bloom obj quad (Node area objs nodes depth) = Node area objs nodes' depth
  where bloomed = Node (Quadrant.slice quad area) [obj] [Bud, Bud, Bud, Bud] (depth + 1)
        nodeI = if quad == Quadrant.I then bloomed else nodes !! 0
        nodeII = if quad == Quadrant.II then bloomed else nodes !! 1
        nodeIII = if quad == Quadrant.III then bloomed else nodes !! 2
        nodeIV = if quad == Quadrant.IV then bloomed else nodes !! 3
        nodes' = [nodeI, nodeII, nodeIII, nodeIV]

-- | If the quadrant is only a bud, then create a new node.
insertOrBloom :: Entity -> Quadrant.Quadrant -> QuadTree -> QuadTree
insertOrBloom obj quad tree = case (subnode quad tree) of
  Bud -> bloom obj quad tree
  Node _ _ _ _ -> insert [obj] (subnode quad tree)

-- | recursively places objects in structure
put :: [Entity] -> QuadTree -> QuadTree
put objs tree
  | size == 0 = tree
  | size > 1 = put (tail objs) (put [head objs] tree)
  | otherwise = if (quadrant == Quadrant.Root) then append objs tree else insertOrBloom (head objs) quadrant tree
  where size = length objs
        quadrant = Quadrant.partit (coord (head objs)) (areaCovered tree)

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
insert objs tree
  | count > splitThreshold = put objs tree
  | otherwise = append objs tree
  where count = length objs + length (objects tree)

-- | Returns number of objects in tree.
mass :: QuadTree -> Int
mass tree = case tree of Bud -> 0
                         Node _ objects nodes _ -> (length objects) + sum (map mass nodes)

-- | Returns partition corresponding to point
partit :: [Double] -> QuadTree -> (Quadrant.Quadrant, Rectangle.Rectangle, QuadTree)
partit pt tree = case tree of
  Bud -> (Quadrant.Root, Rectangle.Rectangle [] [], tree)
  Node area _ _ _ -> let quad = Quadrant.partit pt area
                         node = subnode quad tree
                     in if (node == Bud || quad == Quadrant.Root)
                        then (quad, (Quadrant.slice quad area), tree)
                        else (partit pt node)
