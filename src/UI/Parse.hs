{-------------------------------------------------------------------------------
  UI/Parse.hs
  Author: Kathryn McKay

  Interpets input strings from user into data.
-------------------------------------------------------------------------------}
{-|
  Functions that handle interactivity and access to things.
-}
module UI.Parse (
  CommandType(..)
, helpContent
, parseCommand
) where

{-------------------------------------------------------------------------------
  Imports
-------------------------------------------------------------------------------}
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

{-------------------------------------------------------------------------------
  Exported types
-------------------------------------------------------------------------------}
-- | Enumeration of commands
data CommandType = PrintHelpCommand
  | PrintSceneCommand
  | QueryPartitionCommand
  | CommandError
  deriving (Enum, Eq, Ord)

{-------------------------------------------------------------------------------
  Private data
-------------------------------------------------------------------------------}
-- | Strings versus codes
namedCommands :: Map.Map String CommandType
namedCommands = Map.fromList [("print", PrintSceneCommand)
  , ("help", PrintHelpCommand)
  , ("point", QueryPartitionCommand)]

-- | Codes versus flavour text
commandDescriptions :: Map.Map CommandType String
commandDescriptions = Map.fromList [(PrintSceneCommand, "output information on scene")
  , (PrintHelpCommand, "show this list of commands")
  , (QueryPartitionCommand, "retrieve corresponding partition for a given x, y")]

-- |
getCommandDescription :: String -> String
getCommandDescription name
  | command == CommandError = "ERROR: couldn't match name to command"
  | otherwise = Maybe.fromMaybe "ERROR: couldn't match command to description"
    (Map.lookup command commandDescriptions)
  where command = Maybe.fromMaybe CommandError (Map.lookup name namedCommands)

{-------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------------------------}
-- | string containing newline separated names of all commands
helpContent :: String
helpContent = concat $ map (\x -> x ++ " - " ++ show (getCommandDescription x) ++ "\n")
  (Map.keys namedCommands)

parseCommand :: String -> CommandType
parseCommand str = Maybe.fromMaybe CommandError (Map.lookup str namedCommands)
