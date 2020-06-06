twoPowersList :: Int->[Int]
helper :: [Int]->Int->Int->[Int]

twoPowersList n = helper [] 1 n

helper list cur_num 0 = reverse list
helper list cur_num n = helper (cur_num : list) (2 * cur_num) $ n - 1
