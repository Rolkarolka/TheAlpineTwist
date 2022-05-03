module Utilities where
    split :: (Char -> Bool) -> String -> [String]
    split p s = case dropWhile p s of
                    "" -> []
                    s' -> w : split p s''
                            where (w, s'') = break p s'