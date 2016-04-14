import QuadTree

quadTree1 = QuadTreeNode { size = (10.0, 10.0), objects = [],
  children = (QuadTreeTip, QuadTreeTip, QuadTreeTip, QuadTreeTip),
  depth = 0 }

main = do
  putStrLn " I think it's working"
  putStrLn $ show quadTree1
  putStrLn $ "height: " ++ show (quadTreeHeight quadTree1)