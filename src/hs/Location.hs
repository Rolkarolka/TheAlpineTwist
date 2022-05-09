module Location where
    import Prelude
    import qualified Data.List as List

    import Person
    import Animal
    import State

    data Direction = North | West | East | South
        deriving (Read, Show, Enum, Eq)

    data Path = Path {
        from :: String,
        by :: Direction,
        to :: String
        } deriving (Show)

    paths =
        [
            Path "room_of_thomas_and_giulia" West "corridor",
            Path "corridor" East "room_of_thomas_and_giulia",
            Path "corridor" West "room_of_zoe",
            Path "corridor" South "reception",
            Path "room_of_zoe" East "corridor",
            Path "reception" North "corridor",
            Path "reception" West "bar",
            Path "reception" South "hotel_entrance",
            Path "bar" East "reception",
            Path "bar" South "kitchen",
            Path "hotel_entrance" North "reception",
            Path "hotel_entrance" South "hunters_shaque",
            Path "kitchen" North "bar",
            Path "kitchen" East "hunters_shaque",
            Path "hunters_shaque" North "hotel_entrance",
            Path "hunters_shaque" West "kitchen"
        ]

    go state direction = do
        case List.find (\x -> from x == i_am_at state && by x == direction) paths of
            Nothing -> state { message = ["You can't go that way!"] }
            Just path -> noticeAnimal (noticePeople (state { i_am_at = to path, talking_to = "nobody" }))

