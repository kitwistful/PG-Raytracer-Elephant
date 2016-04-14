import QuadTree

quadTree1 = QuadTreeSplitNode
  (QuadTreeEmpty 1)
  (QuadTreeEmpty 1)
  (QuadTreeEmpty 1)
  (QuadTreeEmpty 1)
  0

quadTree2 = QuadTreeSplitNode
  (QuadTreeSplitNode (QuadTreeEmpty 2) (QuadTreeEmpty 2) (QuadTreeEmpty 2) (QuadTreeEmpty 2) 1)
  (QuadTreeEmpty 1)
  (QuadTreeEmpty 1)
  (QuadTreeEmpty 1)
  0

main = do
  putStrLn "Quad Tree 1"
  print quadTree1
  putStrLn "Quad Tree 2"
  print quadTree2