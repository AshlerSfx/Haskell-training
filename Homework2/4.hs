find :: Int->[Int]->Int
helper :: [Int]->Int->Int->Int 

helper [] _ _ = 0
helper (x : xs) num pos 
                       | (x == num) = pos
                       | otherwise = helper xs num (pos + 1)

find n list = helper list n 1

