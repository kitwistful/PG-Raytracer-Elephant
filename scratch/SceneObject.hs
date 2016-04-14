module SceneObject (
hallo
, Vector(..)
, tupleSummation
, printVectors
) where

hallo = do
  putStrLn "hallo there"
  
data Vector = Vector { x :: Float
                      , y :: Float
                      } 
            
tupleSummation :: (Int, Int) -> Int
tupleSummation (x, y) = x + y

printVectors :: [(Int, Int)] -> IO ()
printVectors vectors = do
  mapM_ print vectors