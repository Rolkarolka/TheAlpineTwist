module Utilities where
    split :: (Char -> Bool) -> String -> [String]
    split p s = case dropWhile p s of
                    "" -> []
                    s' -> w : split p s''
                            where (w, s'') = break p s'

    scanChar :: Char -> Int
    scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
               | otherwise = -1

    scanString :: String -> Int
    scanString = go 0
        where go a [] = a
              go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                          | otherwise = 0
                  where sc = scanChar x

    removeItem _ []                 = []
    removeItem x (y:ys) | x == y    = removeItem x ys
                        | otherwise = y : removeItem x ys