digitSum :: Int->Int
helper :: Int->Int->Int

digitSum n = helper 0 (abs n)

helper acc n = if(div n 10 == 0) then n + acc else helper (acc + mod n 10) $div n 10
