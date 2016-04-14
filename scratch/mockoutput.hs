import SceneObject

printTitle title = do
  putStrLn title
  putStrLn "----"
  
coord = Vector {x = 1.5, y = 24.4}

main = do
  printTitle "DIAGRAM:"
  hallo
  print $ x coord
  print $ y coord

  printTitle "FOUND OBJECTS:"
  printTitle "QUADTREE:"
  print $ tupleSummation (1, 2)
  printTitle "OBJECTS:"
  printVectors [(1, 2), (3, 4), (5, 6)]