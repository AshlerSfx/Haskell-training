digitSum :: Int->Int
helper :: Int->Int->Int

digitSum n = helper n 0

helper 0 s = s
helper x s = helper (div x 10) s + (mod x 10)
