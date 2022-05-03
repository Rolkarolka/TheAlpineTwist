module Person where
    import Prelude
    import qualified Data.List as List

    import State

    talk state person = do
        let maybePerson = (List.find (\(x) -> person == fst x) (people_at state))
        case maybePerson of
            Nothing -> state { message = ["You try talking to your new imaginary friend, but she/he isn't responding."] }
            Just(realPerson) -> if (snd realPerson == (i_am_at state)) then
                                state { talking_to = person }
                            else
                                state { message = ["You start to formulate your sentence towards " ++ person ++ ", when suddenly you realise, that he cannot hear you, for she/he isn't here."] }

    noticePeople state = state { message = ["You notice following people around you: "] ++ (map (\x -> fst x) (filter (\x -> snd x == (i_am_at state)) (people_at state))) }
