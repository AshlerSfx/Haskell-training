

listDec :: Int -> [[Int]]
listDec n = helper n n
    where
        helper 0 _ = [[]]
        helper a b | a < b     = helper a a
                   | otherwise = concatMap (\t -> map (\s -> t : s) $ helper (a - t) t) [1, 2..b]
