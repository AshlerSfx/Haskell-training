findMaxSum :: [Int]->Int
findMaxSum string = helper string 0 1 (-1)

helper [] _ _ pos = pos

helper (x : xs) max cur pos 
    |(xs == []) = pos
    |cur == 1 = helper xs sum (cur + 1) 1  
    |sum > max = helper xs sum (cur + 1) cur
    |otherwise = helper xs max (cur + 1) pos
    where sum = head(xs) + x
