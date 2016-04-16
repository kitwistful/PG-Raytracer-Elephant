import QuadTree

import Data.FixedList

quadTree1 = quadTreeRoot 1.0 0.0 19.0 10.0

main = do
  putStrLn $ show quadTree1
  putStrLn $ "height: " ++ show (quadTreeHeight quadTree1)
  putStrLn $ concat (map (\x -> show x 
                              ++ " " 
                              ++ show (quadrantRange (areaCovered quadTree1) x)
                              ++ "\n")
                      [(FirstQuadrant)..(FourthQuadrant)])
  putStrLn $ concat (map (\x -> show x
                             ++ " falls in "
                             ++ show (pointFallsIntoQuadrant (areaCovered quadTree1) x)
                             ++ "\n")
                      [[0.0, 0.0], [9.5, 1.0], [1.0, 5.0], [9.5, 5.0], [9.0, 9.9], [4.9, 0.0]])