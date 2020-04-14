fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = helper n 1 where
    helper :: Int->Int->Int
    helper 0 s  = s
    helper n s  = helper (n-1) s*n

