-- TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek.

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
