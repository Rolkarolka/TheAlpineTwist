-- TheAlpineTwist, by Ksawery Chodyniecki, Karolina Romanowska and Grzegorz Rusinek.

import Prelude hiding (take)
import qualified Data.List as List

import Fact
import Item
import Location
import Person
import Animal
import State
import Utilities

describeIntroduction = [
        "",
        "Daily life in the alpine hotel was disrupted by the barking of the Promyczek. The wife, which came after that to the room, saw her husband on the floor. She started screaming and woke up everyone who stayed in the hotel. The hotel owner immediately calls you to solve that riddle. Who is the murderer? You don't know. Yet...",
        ""
    ]

describeStandardHelp = 
    [ "Available commands are:"
    , "help / h      -- to see available commands."
    , "talk person   -- talk to \"person\"."
    , "crouch animal -- crouch to \"animal\""
    , "take item     -- take an \"item\" and add it to your inventory."
    , "look / l      -- notice all people located in the current room."
    , "notice / n    -- notice all items located in the current room."
    , "inventory / i -- list all owned items."
    , "journal / j   -- list all known facts."
    , "quit          -- to end the game and quit."
    , "w             -- go up / north."
    , "d             -- go right / east."
    , "a             -- go left / west."
    , "s             -- go down / south."
    , ""
    ]

describeDialogueHelp = 
    [ "Available commands are:"
    , "help / h      -- to see available commands."
    , "tellAbout fact -- ask about \"fact\"."
    , "askAbout item -- ask about \"item\"."
    , "gossipAbout person -- ask about \"person\"."
    , "whyHere       -- ask your interlocutor why is she/he here."
    , "accuse person -- accuse a \"person\" of murder."
    , "inventory / i -- list all owned items."
    , "journal / j   -- list all known facts."
    , "quit          -- to end the game and quit."
    , "w             -- go up / north."
    , "d             -- go right / east."
    , "a             -- go left / west."
    , "s             -- go down / south."
    , ""
    ]

describeCrouchHelp = 
    [ "Available commands are:"
    , "help / h      -- to see available commands."
    , "play          -- play ball with the \"animal\""
    , "pet           -- pet the \"animal\""
    , "take item     -- take an \"item\" and add it to your inventory."
    , "look / l      -- notice all people located in the current room."
    , "notice / n    -- notice all items located in the current room."
    , "inventory / i -- list all owned items."
    , "journal / j   -- list all known facts."
    , "quit          -- to end the game and quit."
    , "w             -- go up / north."
    , "d             -- go right / east."
    , "a             -- go left / west."
    , "s             -- go down / south."
    , ""
    ]


printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

printState :: State -> IO ()
printState state = do
    putStr (unlines (message state))
    putStr ("You're at " ++ (i_am_at state))
    if not ((crouching_to state == "nobody")) then
        putStr (", you're crouching to " ++ (crouching_to state) ++ "\n")
    else
        putStr (", you're talking to " ++ (talking_to state) ++ "\n")

help state = 
    if not ((talking_to state) == "nobody") then
        state { message = describeDialogueHelp }
    else 
        if not ((crouching_to state == "nobody")) then
            state { message = describeCrouchHelp }
        else
            state { message = describeStandardHelp }

introduction = printLines describeIntroduction

readCommand :: IO String
readCommand = do
    xs <- getLine
    return xs

gameLoop :: State -> IO State
gameLoop state = do
    printState state
    let newState = state { message = [""] }
    putStr "\nWaiting for command:\n> "
    cmd <- readCommand
    if not (cmd == "quit") then
        gameLoop (case cmd of
            "help" -> help newState
            "h" -> help newState
            "look" -> noticeAnimal (noticePeople newState)
            "l" -> noticeAnimal (noticePeople newState)
            "notice" -> noticeItems newState
            "n" -> noticeItems newState
            "inventory" -> inventory newState
            "i" -> inventory newState
            "journal" -> journal newState
            "whyHere" -> whyHere newState
            "j" -> journal newState
            "w" -> go newState North
            "d" -> go newState East
            "a" -> go newState West
            "s" -> go newState South
            _ -> if List.isPrefixOf "take" cmd then take newState ((split (==' ') cmd)!!1)
                 else if List.isPrefixOf "talk" cmd then talk newState ((split (==' ') cmd)!!1)
                 else if List.isPrefixOf "crouch" cmd then crouch newState ((split (==' ') cmd)!!1)
                 else if List.isPrefixOf "tellAbout" cmd then tellAbout newState ((split (==' ') cmd)!!1)
                 else if List.isPrefixOf "askAbout" cmd then askAbout newState ((split (==' ') cmd)!!1)
                 else if List.isPrefixOf "gossipAbout" cmd then gossipAbout newState ((split (==' ') cmd)!!1)
                 else if List.isPrefixOf "accuse" cmd then accuse newState ((split (==' ') cmd)!!1)
                 else newState { message = ["Unknown command"] }
            )
        else do 
            printLines ["Goodbye"]
            return(newState)

main :: IO State
main = do
    introduction
    gameLoop (help (State []
        -- i_am_at
        "room_of_thomas_and_giulia"
        -- people_at
        [ ("thomas", "room_of_thomas_and_giulia")
        , ("giulia", "room_of_thomas_and_giulia")
        , ("andreas", "room_of_thomas_and_giulia")
        , ("zoe", "room_of_zoe")
        , ("karl", "bar")
        , ("amy", "bar")
        , ("stephan", "bar")
        , ("jurgen", "corridor")
        , ("hilda", "corridor")
        , ("theodor", "kitchen")
        , ("hans", "reception")
        , ("hermann", "reception")
        , ("jonas", "hotel_entrance")
        , ("urlich", "hotel_entrance")
        ]
        -- animals_at
        [ ("promyczek", "reception")
        ]
        -- items_at
        [ ("watch", "room_of_thomas_and_giulia")
        , ("thomas_journal", "room_of_thomas_and_giulia")
        , ("cigarette_light", "room_of_thomas_and_giulia")
        , ("sleep_mask", "room_of_zoe")
        , ("sleeping_pills", "room_of_zoe")
        , ("cup", "room_of_zoe")
        , ("clubs_symbol", "karl")
        , ("glass", "bar")
        , ("brooch", "hilda")
        , ("cleaning_stuff", "corridor")
        , ("cutlery_tray", "corridor")
        , ("guest_book", "reception")
        , ("telephone", "reception")
        , ("ball", "reception")
        , ("hunting_weapon", "hermann")
        , ("gilded_epaulettes", "urlich")
        , ("bush", "hotel_entrance")
        , ("bullets", "hunters_shaque")
        , ("knife_scabbard", "hunters_shaque")
        , ("blooded_knife", "hunters_shaque")
        , ("deer", "kitchen")
        , ("broth", "kitchen")
        ]
        -- known_facts
        [ "thomas_had_been_murdered"
        ]
        -- holding
        [ "money"
        ]
        -- talking_to
        "nobody"
        -- crouching to
        "nobody"
        ))
