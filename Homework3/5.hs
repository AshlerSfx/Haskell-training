fibonacci :: Int -> Int

helper :: Int -> Int -> Int -> Int

fibonacci n =  helper 0 1 n

helper val1 val2 n 
  | n > 0 = helper val2 (val1 + val2) (n - 1)
  | n < 0 = helper (val2 - val1) val1 (n + 1)
  | otherwise = val1
