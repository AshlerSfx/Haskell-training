twoPowersList :: Int->[Int]
helper :: [Int]->Int->Int->[Int]

twoPowersList n = helper [] 1 n

helper list cur_num 0 = list ++ [cur_num]
helper list cur_num n = helper (list ++ [cur_num]) (2*cur_num) $n-1
