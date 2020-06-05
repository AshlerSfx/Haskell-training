withMap :: [Int] -> Int
withMap list = sum (map (\x -> if even x then 1 else 0) list)

withFilter :: [Int] -> Int
withFilter list = length $ filter even list

withFoldr :: [Int] -> Int
withFoldr = foldr (\x t -> if even x then t + 1 else t) 0
