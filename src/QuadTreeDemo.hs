{-------------------------------------------------------------------------------
  QuadTreeDemo.hs
  Author: Kathryn McKay

  Starts up a preloaded scene where the user can test different (x,y) points
  to see what partitions they lie in, as well as what entities are found.
-------------------------------------------------------------------------------}
import qualified UI.Parse as Parse
import qualified Scene.QuadTree as QuadTree
import qualified Scene.Rectangle as Rectangle
import Scene.Entity
import Control.Monad
import System.IO

scene :: QuadTree.QuadTree
scene = QuadTree.insert sceneObjects $ QuadTree.create  $ Rectangle.Rectangle [-10.0, -10.0] [20.0, 20.0]

handleInput :: Parse.CommandType -> IO ()
handleInput command
  | command == Parse.CommandError = putStrLn "Error parsing input"
  | otherwise = case command of
    Parse.PrintHelpCommand -> putStrLn Parse.helpContent
    Parse.PrintSceneCommand -> putStrLn $ "[ Scene ]" ++ show scene
    Parse.QueryPartitionCommand -> putStrLn "enter a point I guess\nActually nvm" -- TODO: after scene prints with 'print'

sceneObjects :: [Entity]
sceneObjects = [(Entity "Object A" [-5, -5]), (Entity "Object B" [-5, 0]), (Entity "Object C" [5, 0]), (Entity "Object D" [0, 5]), (Entity "Object E" [0, -5])]

main :: IO ()
main = do
  -- Load initial scene
  handleInput Parse.PrintSceneCommand

  -- Print commands
  handleInput Parse.PrintHelpCommand

-- Parse commands forever
  forever $ do
    putStr "> "
    hFlush stdout
    currentLine <- getLine
    handleInput $ Parse.parseCommand currentLine
