module Item where
    import Prelude
    import qualified Data.List as List

    import State
    import Data.Map (findWithDefault, fromList, (!))

    take state item =
        if item `elem` holding state then
            state { message = ["You take the " ++ item ++ " out of your bag, then put it on the table, and after that you take it and put it in your pocket."] }
        else do
            let maybeThing = List.find (\x -> item == fst x) (items_at state)
            case maybeThing of
                Nothing -> state { message = ["You try to grab the " ++ item ++ " or at least you try to grasp your hallucination of it."] }
                Just realThing -> if snd realThing == i_am_at state then
                                  state { holding = item:holding state, items_at = List.delete realThing (items_at state) }
                              else
                                  state { message = ["You try to grab the " ++ item ++ " or at least you try to grasp your hallucination of it."] }

    noticeItems state = state { message = "You notice following items laying around: " : map fst (filter (\x -> snd x == i_am_at state) (items_at state)) }

    noticeTrinkets state = state { message = "You notice following items on him/her: " : map fst (filter (\x -> snd x == talking_to state) (items_at state)) }

    inventory state = state { message = "You have these items: " : map (`describeItem` state) (holding state) }

    describeItem :: String -> State -> [Char]
    describeItem item state = if item `elem` Prelude.map fst (amount state) then
            item ++ " x" ++ show (findWithDefault 1 item (fromList (amount state)))
        else
            item