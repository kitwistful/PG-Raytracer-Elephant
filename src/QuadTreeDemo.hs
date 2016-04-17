{-------------------------------------------------------------------------------
  QuadTreeDemo.hs
  Author: Kathryn McKay

  Starts up a preloaded scene where the user can test different (x,y) points
  to see what partitions they lie in, as well as what entities are found.
-------------------------------------------------------------------------------}
import qualified UI.Parse as Parse
import Control.Monad
import System.IO

handleInput :: Parse.CommandType -> IO ()
handleInput command
  | command == Parse.CommandError = putStrLn "Error parsing input"
  | otherwise = case command of
    Parse.PrintHelpCommand -> putStrLn Parse.helpContent
    Parse.PrintSceneCommand -> putStrLn "print scene" -- TODO: when implementing tree
    Parse.QueryPartitionCommand -> putStrLn "enter a point I guess\nActually nvm" -- TODO: after scene prints with 'print'

main :: IO ()
main = do
  -- Load initial scene
  -- TODO: when implemented tree

  -- Print commands
  putStrLn Parse.helpContent

-- Parse commands forever
  forever $ do
    putStr "> "
    hFlush stdout
    currentLine <- getLine
    handleInput $ Parse.parseCommand currentLine
