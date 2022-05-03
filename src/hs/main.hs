-- TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek.

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

data Direction = North | West | East | South
    deriving (Read, Show, Enum)

paths = Map.fromList [("room_of_thomas_and_giulia", "w")]

describeIntroduction = [
    "Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You don't know. Yet..."
    ]

describeHelp = [
    "Available commands are:",
    "",
    "help          -- to see these instructions.",
    "quit          -- to end the game and quit.",
    ""
    ]

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

help = printLines describeHelp
introduction = printLines describeIntroduction

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs
    
-- note that the game loop may take the game state as
-- an argument, eg. gameLoop :: State -> IO ()
gameLoop :: IO ()
gameLoop = do
    cmd <- readCommand
    case cmd of
        "help" -> do printLines describeHelp
                     gameLoop
        "quit" -> return ()
        _ -> do printLines ["Unknown command.", ""]
                gameLoop

main = do
    introduction
    help
    gameLoop
