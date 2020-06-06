sumThree :: [Int]->[Int]->[Int]->[Int]
sumTwo :: [Int]->[Int]->[Int]

sumTwo a b = helper a b []
 
helper [] [] list = reverse list
helper (x : xs) [] list = helper xs [] (x : list)
helper [] (y : ys) list = helper [] ys (y : list)
helper (x : xs) (y : ys) list = helper xs ys ((x + y) : list)

sumThree xs ys zs = sumTwo xs (sumTwo ys zs) 

