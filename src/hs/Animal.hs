module Animal where
    import qualified Data.Maybe as Maybe
    import qualified Data.List as List

    import State

    animal_descriptions =
        [   ("promyczek", "German Spitz Miniature with keen-looking eyes. It likes caresses and it\'s well trained.")
        ]
    noticeAnimal state = state { message = (message state) ++ ["You notice following animals around you: "]  ++ (map (\x -> fst x) (filter (\x -> snd x == (i_am_at state)) (animals_at state))) }

    crouch :: State -> String -> State
    crouch state animal = do
        let maybeAnimal = (List.find (\(x) -> animal == fst x) (animals_at state))
        (case maybeAnimal of
            Nothing -> state { message = ["You try crouching to your new imaginary pet, but it isn't responding."] }
            Just(realAnimal) -> if (snd realAnimal == (i_am_at state)) then do
                                let newState = state { crouching_to = animal, message = [snd (Maybe.fromJust (List.find (\(x) -> animal == fst x) animal_descriptions))] }
                                newState { message = (message newState) }
                            else
                                state { message = ["You start to reaching " ++ animal ++ ", when suddenly you realise, that it cannot be touched, for it isn't here."] })
