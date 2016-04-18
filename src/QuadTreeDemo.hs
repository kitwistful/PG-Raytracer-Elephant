{-------------------------------------------------------------------------------
  QuadTreeDemo.hs
  Author: Kathryn McKay

  Starts up a preloaded scene where the user can test different (x,y) points
  to see what partitions they lie in, as well as what entities are found.

  Usage: run main to start repl
-------------------------------------------------------------------------------}
import qualified UI.Parse as Parse
import qualified Scene.QuadTree as QuadTree
import qualified Scene.Rectangle as Rectangle
import Scene.Entity
import Control.Monad
import System.IO

-- | Collection of objects in scene
sceneObjects :: [Entity]
sceneObjects = [(Entity "Object A" [-5, -5]), (Entity "Object B" [-5, 0]), (Entity "Object C" [5, 0]), (Entity "Object D" [0, 5]), (Entity "Object E" [0, -5])]

-- | Objects after they are put in scene
scene :: QuadTree.QuadTree
scene = QuadTree.insert sceneObjects $ QuadTree.create  $ Rectangle.Rectangle [-10.0, -10.0] [20.0, 20.0]

-- | Asks user for point coords to test scene with
handleQueryPartition :: IO ()
handleQueryPartition = do
  putStr "Enter x coord: "
  hFlush stdout
  x <- getLine
  let x' = read x :: Double
  putStr "Enter y coord: "
  hFlush stdout
  y <- getLine
  let y' = read y :: Double
  let (quad, area, tree) = QuadTree.partit [x', y'] scene
  putStrLn $ "\n[ Result ]\nFor point: " ++ show [x', y']
  putStrLn $ "Hit node:\n" ++ show tree
  putStrLn $ "Quadrant: " ++ show quad
  putStrLn $ "Spans area: " ++ show area

-- | A nice switch statement for handling specific commadns.
-- | Also responsible for explaining what happens when the command wasn't any
-- | good.
handleInput :: Parse.CommandType -> IO ()
handleInput command
  | command == Parse.CommandError = putStrLn "Error parsing input"
  | otherwise = case command of
    Parse.PrintHelpCommand -> putStrLn Parse.helpContent
    Parse.PrintSceneCommand -> putStrLn $ "[ Scene ]\n" ++ show scene
    Parse.QueryPartitionCommand -> handleQueryPartition

-- | Main function.
main :: IO ()
main = do
  -- Print helpful things
  putStrLn "Quad Tree Demo: 'help' for command list"

-- Parse commands forever
  forever $ do
    putStr "> "
    hFlush stdout
    currentLine <- getLine
    handleInput $ Parse.parseCommand currentLine
