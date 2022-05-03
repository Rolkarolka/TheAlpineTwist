module State where
    data State = State {
            message :: [String],
            i_am_at :: String,
            people_at :: [(String, String)],
            animals_at :: [(String, String)],
            items_at :: [(String, String)],
            known_facts :: [String],
            holding :: [String],
            talking_to :: String
        } deriving (Show)