{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Animal where
    import qualified Data.Maybe as Maybe
    import qualified Data.List as List

    import State

    animalDescriptions = [
         ("promyczek", "German Spitz Miniature with keen-looking eyes. It likes caresses and it\'s well trained.")]
    playingAsciiArt = [
        "Hops, hops, hops. The ball is rolling on the floor, and you watch animal - a small, fluffy animal chasing the ball.",
        "               ;~~,__",
        ":-....,-------'`-'._.'",
        " `-,,,  ,       ;'~~'",
        "   ,'_,'~.__; '--.",
        "  //'       ````(;",
        " `-'                           O"]
    noticeAnimal state = state { message = message state ++ ["You notice following animals around you: "]  ++ map fst (filter (\x -> snd x == i_am_at state) (animals_at state)) }

    crouch :: State -> String -> State
    crouch state animal = do
        let maybeAnimal = List.find (\x -> animal == fst x) (animals_at state)
        (case maybeAnimal of
            Nothing -> state { message = ["You try crouching to your new imaginary pet, but it isn't responding."] }
            Just realAnimal -> if snd realAnimal == i_am_at state then do
                            let newState = state { crouching_to = animal, message = [snd (Maybe.fromJust (List.find (\x -> animal == fst x) animalDescriptions))] }
                            newState { message = message newState }
                          else
                            state { message = ["You start to reaching " ++ animal ++ ", when suddenly you realise, that it cannot be touched, for it isn't here."] })

    internalCrouch state action = do
        let maybeAnimal = List.find (\x -> crouching_to state == fst x) (animals_at state)
        (case maybeAnimal of
            Nothing -> state { message = ["You try crouching to your new imaginary pet, but it isn't responding."] }
            Just realAnimal -> if i_am_at state == snd realAnimal then do
              (case action of
                  "pet" -> state { message = ["Yayy! You start petting " ++ crouching_to state ++ ", and you enjoy it."] }
                  "play" ->
                      if "ball" `elem` holding state then
                          state { message = ("Yayy! You start playing with the " ++ crouching_to state ++ ", and you enjoy it.") : playingAsciiArt }
                      else
                          state { message = ["You want to play with the " ++ crouching_to state ++ ", but you don't have its favorite ball."] })
              else
                  state { message = ["You start to reaching " ++ crouching_to state ++ ", when suddenly you realise, that it cannot be touched, for it isn't here."] })


    pet state animal =
        internalCrouch state "pet"

    play state animal =
        internalCrouch state "play"
