withMap :: [Int] -> Int
withMap  = sum.map (\x -> if even x then 1 else 0) 
--scary brackets deleted & arg deleted from both sides
withFilter :: [Int] -> Int
withFilter list = length $ filter even list

withFoldr :: [Int] -> Int
withFoldr = foldr (\x t -> if even x then t + 1 else t) 0
