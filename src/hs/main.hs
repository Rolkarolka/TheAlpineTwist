-- TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek.

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

data Direction = North | West | East | South
    deriving (Read, Show, Enum, Eq)

data Path = Path {
    from :: String,
    by :: Direction,
    to :: String
    } deriving (Show)

data State = State {
    message :: [String],
    i_am_at :: String
    } deriving (Show)

paths =
    [
        Path "room_of_thomas_and_giulia" West "corridor",
        Path "corridor" East "room_of_thomas_and_giulia",
        Path "corridor" West "room_of_zoe",
        Path "corridor" South "reception",
        Path "room_of_zoe" East "corridor",
        Path "reception" North "corridor",
        Path "reception" West "bar",
        Path "reception" South "hotel_entrance",
        Path "bar" East "reception",
        Path "bar" South "kitchen",
        Path "hotel_entrance" North "reception",
        Path "hotel_entrance" South "hunters_shaque",
        Path "kitchen" North "bar",
        Path "kitchen" East "hunters_shaque",
        Path "hunters_shaque" North "hotel_entrance",
        Path "hunters_shaque" West "kitchen"
    ]

go state direction = do
    case (List.find (\(x) -> from x == i_am_at state && by x == direction) paths) of
        Nothing -> state { message = ["You can't go that way!"] }
        Just(path) -> state { i_am_at = to path }
    -- look

describeIntroduction = [
        "",
        "Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You don't know. Yet...",
        ""
    ]

describeHelp = [
    "Available commands are:",
    "help          -- to see these instructions.",
    "quit          -- to end the game and quit.",
    "w             -- go up / north.",
    "d             -- go right / east.",
    "a             -- go left / west.",
    "s             -- go down / south.",
    ""
    ]

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printState :: State -> IO ()
printState state = do
    putStr (unlines (message state))
    print (i_am_at state)

help state = state { message = describeHelp }
introduction = printLines describeIntroduction

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

gameLoop :: State -> IO State
gameLoop state = do
    printState state
    let newState = state { message = [""] }
    printLines ["", "Waiting for command:"]
    cmd <- readCommand
    if not (cmd == "quit") then
        gameLoop (case cmd of
            "help" -> help newState
            "w" -> go newState North
            "d" -> go newState East
            "a" -> go newState West
            "s" -> go newState South
            _ -> newState
            )
        else do 
            printLines ["Goodbye"]
            return(newState)

main :: IO State
main = do
    introduction
    gameLoop (help (State [""] "room_of_thomas_and_giulia"))
