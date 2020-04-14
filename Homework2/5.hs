sumThree :: [Int]->[Int]->[Int]->[Int]

sumThree a b c = sumTwo [] c $sumTwo [] a b where

    sumTwo :: [Int]->[Int]->[Int]->[Int]

    sumTwo newlist [] [] = newlist
    sumTwo newlist [] b = newlist ++ b
    sumTwo newlist a [] = newlist ++ a 
    sumTwo newlist (x:as) (y:bs) = sumTwo (newlist ++ [x + y]) as bs
