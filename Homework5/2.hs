listOfPairs :: Int -> [Int]
listOfPairs n = [1..n] >>= \t -> map (*t) [1..n]
