module Item where
    import Prelude
    import qualified Data.List as List

    import State

    take state item =
        if elem item (holding state) then
            state { message = ["You take the " ++ item ++ " out of your bag, then put it on the table, and after that you take it and put it in your pocket."] }
        else do
            let maybeThing = (List.find (\(x) -> item == fst x) (items_at state))
            case maybeThing of
                Nothing -> state { message = ["You try to grab the " ++ item ++ " or at least you try to grasp your hallucination of it."] }
                Just(realThing) -> if (snd realThing == (i_am_at state)) then
                                    state { holding = item:(holding state), items_at = (List.delete realThing (items_at state)) }
                                else
                                    state { message = ["You try to grab the " ++ item ++ " or at least you try to grasp your hallucination of it."] }

    notice state = state { message = ["You notice following items laying around: "] ++ (map (\x -> fst x) (filter (\x -> snd x == (i_am_at state)) (items_at state))) }

    inventory state = state { message = ["You have these items: "] ++ holding state }
