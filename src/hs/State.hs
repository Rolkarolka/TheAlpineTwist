module State where
    data State = State {
            message :: [String],
            i_am_at :: String
        } deriving (Show)