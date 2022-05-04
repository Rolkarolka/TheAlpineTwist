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

    internalCrouch state action = do
        let maybeAnimal = (List.find (\(x) -> (crouching_to state) == fst x) (animals_at state))
        (case maybeAnimal of
            Nothing -> state { message = ["You try crouching to your new imaginary pet, but it isn't responding."] }
            Just(realAnimal) -> if ((i_am_at state) == (snd realAnimal)) then do
                (case action of
                    "pet" -> state { message = ["Yayy! You start petting " ++ (crouching_to state) ++ ", and you enjoy it."] }
                    "play" ->
                        if (elem "ball" (holding state)) then
                            state { message = ["Yayy! You start playing with the " ++ (crouching_to state) ++ ", and you enjoy it."] }
                        else
                            state { message = ["You want to play with the " ++ (crouching_to state) ++ ", but you don't have its favorite ball."] })  
                else
                    state { message = ["You start to reaching " ++ (crouching_to state) ++ ", when suddenly you realise, that it cannot be touched, for it isn't here."] })


    pet state animal =
        internalCrouch state "pet" 
    
    play state animal =
        internalCrouch state "play"
