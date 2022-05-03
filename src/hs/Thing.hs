module Thing where
    import Prelude
    import qualified Data.List as List

    import State

    take state thing =
        if elem thing (holding state) then
            state { message = ["You take the " ++ thing ++ " out of your bag, then put it on the table, and after that you take it and put it in your pocket."] }
        else do
            let maybeThing = (List.find (\(x) -> thing == fst x) (things_at state))
            case maybeThing of
                Nothing -> state { message = ["You try to grab the " ++ thing ++ " or at least you try to grasp your hallucination of it."] }
                Just(realThing) -> if (snd realThing == (i_am_at state)) then
                                    state { holding = thing:(holding state), things_at = (List.delete realThing (things_at state)) }
                                else
                                    state { message = ["You try to grab the " ++ thing ++ " or at least you try to grasp your hallucination of it."] }

    notice state = state { message = ["You notice following things laying around: "] ++ (map (\x -> fst x) (filter (\x -> snd x == (i_am_at state)) (things_at state))) }
