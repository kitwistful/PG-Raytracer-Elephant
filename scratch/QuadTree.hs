{-|
  QuadTree module
  version 2
  Author: Kathryn McKay
  
  Exports this space partitioning type for 2D space.
  
  TODO : Note that the cartesian vectors and the children nodes are currently
          stored as dynamically sized vectors, instead of something like a
          fixed-size tuple, when they actually DO have specific constraints on
          their size. After the functions of this module are fully outlined and
          functioning, must assess possible consequences of this approach.
-}
module QuadTree (
  QuadTree(..)
, QuadrantInfo(..)
, Quadrant(..)
, quadTreeHeight
, quadTreeInsert
, quadTreeRoot
, quadrantRange
, quadrantCenter
, pointFallsIntoQuadrant
) where


{-------------------------------------------------------------------------------
  Imports
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  Exported types
-------------------------------------------------------------------------------}

-- | Holds information describing the position and size of a partition.
-- | First param is offset, second param is size.
data QuadrantInfo = QuadrantInfo { offset :: [Double], size :: [Double] }
 deriving (Show)
 
-- | Space partitioning structure.
data QuadTree = QuadTreeTip
  | QuadTreeNode { areaCovered :: QuadrantInfo
                 , objects :: [[Double]]
                 , children :: [QuadTree]
                 , depth :: Int}
                 
data Quadrant = FirstQuadrant | SecondQuadrant | ThirdQuadrant | FourthQuadrant
  | RootQuadrant
 deriving (Show, Enum)
  
{-------------------------------------------------------------------------------
  Private utility functions
-------------------------------------------------------------------------------}
-- Retrieves offset of each partition over area [(0,0) .. (2.0, 2.0))
-- Follows angle partition order i.e. first quadrant is in area (0 degrees, 90
--  degrees) --> upper right corner.
quadrantOffsetFactor :: Quadrant -> [Double]
quadrantOffsetFactor quadrant =
  case quadrant of FirstQuadrant -> [1.0, 0.0]
                   SecondQuadrant -> [0.0, 0.0]
                   ThirdQuadrant -> [0.0, 1.0]
                   FourthQuadrant -> [1.0, 1.0]

{-------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------------------------}

-- | Initializes an empty paritioning space with specified size & offset
quadTreeRoot :: Double -> Double -> Double -> Double -> QuadTree
quadTreeRoot offsetX offsetY sizeX sizeY = QuadTreeNode {
  areaCovered = QuadrantInfo { offset = [offsetX, offsetY],
    size = [sizeX, sizeY] },
  objects = [], children = [QuadTreeTip, QuadTreeTip, QuadTreeTip, QuadTreeTip],
  depth = 0 }
  
-- | Retrieves maximum depth below this node.
quadTreeHeight :: QuadTree -> Int
quadTreeHeight tree = case tree of QuadTreeTip -> 0
                                   tree -> 1 + maximum (map quadTreeHeight
                                          (children tree))

-- | Returns a version of the tree with the object partitioned inside it
quadTreeInsert :: QuadTree -> [Double] -> QuadTree
quadTreeInsert tree object = tree -- TODO: implement after quadrant functions work.


-- | Retrieves the quadrant's offset and size.
quadrantRange :: QuadrantInfo -> Quadrant -> QuadrantInfo
quadrantRange QuadrantInfo {offset=rootOffset, size=rootSize} quadrant
  = QuadrantInfo quadrantOffset quadrantSize
  where quadrantSize = [ x/2 | x <- rootSize]
        quadrantOffset = [(fst x)*(snd x) | x <-
          (zip quadrantSize (quadrantOffsetFactor quadrant))]
          
quadrantCenter :: QuadrantInfo -> [Double]
quadrantCenter quadrant = [((fst x)*2 + (snd x))/2 | x <- (zip (offset quadrant) (size quadrant))]

-- | Retrieves the quadrant which point (x,y) lies on.
-- | If the point lies on the boundary, 'RootQuadrant' is returned.
pointFallsIntoQuadrant :: QuadrantInfo -> [Double] -> Quadrant
pointFallsIntoQuadrant quadrant point
  | x == xBound || y == yBound = RootQuadrant
  | y < yBound && x > xBound = FirstQuadrant
  | y < yBound && x < xBound = SecondQuadrant
  | y > yBound && x > xBound = FourthQuadrant
  | y > yBound && x < xBound = ThirdQuadrant
  where x = point !! 0
        y = point !! 1
        center = quadrantCenter quadrant
        xBound = center !! 0
        yBound = center !! 1
{-------------------------------------------------------------------------------
-- Instance definitions
-------------------------------------------------------------------------------}

-- | Demonstrates tree structure and contents.
instance Show QuadTree where
  show QuadTreeTip = ""
  show QuadTreeNode {areaCovered=areaCovered, objects=objects,
    children=children, depth=depth}
    = "\n" ++ take depth (repeat '│') ++ "├─" ++ show objects
      ++ show children
