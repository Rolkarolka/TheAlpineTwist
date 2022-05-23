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
    describeItem item state = if item `elem` map fst (amount state) then
            item ++ " x" ++ show (findWithDefault 1 item (fromList (amount state)))
        else
            item
    
    setAmount state item newAmount =
        if item `elem` holding state then
            if item `elem` map fst (amount state) then
                state { amount = map (\x -> if fst x == item then (item, newAmount) else x) (amount state), message = ["Amount of " ++ item ++ " changed!"] }
            else
                state { amount = amount state ++ [(item, newAmount)], message = ["Amount of " ++ item ++ " changed!"] }
        else state { message = ["You can't change the amount of an item you don't have."] }

    addAmount state item addAmnt = 
        if item `elem` map fst (amount state) then
            state { amount = map (\x -> if fst x == item then (item, snd x + addAmnt) else x) (amount state), message = ["Amount of " ++ item ++ " increased!"] }
        else state { message = ["You can't change the amount of an item that doesn't have an amount."] }

    subtractAmount state item subAmnt = 
        if item `elem` map fst (amount state) then
            if snd (head (filter (\x -> fst x == item) (amount state))) < subAmnt then
                state { message = ["You don't have enough " ++ item ++ " to subtract that much."] }
            else
                state { amount = map (\x -> if fst x == item then (item, snd x - subAmnt) else x) (amount state), message = ["Amount of " ++ item ++ " decreased!"] }
        else state { message = ["You can't change the amount of an item that doesn't have an amount."] }