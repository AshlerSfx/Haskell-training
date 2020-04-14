miniList = [1,7,9]
list = miniList ++ (concatMap (\x -> zipWith (+) miniList $ replicate 3 $ x*10 ) list)
