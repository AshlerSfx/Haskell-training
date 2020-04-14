fibonacci :: Integer -> Integer
fibonacci n
    | abs n == 1 || n == 0  = abs n
    | n > 0                 = fibonacci(n - 1) + fibonacci(n - 2) 
    | mod n 2 == 0          = - fibonacci(abs n)
    | otherwise             = fibonacci (abs n)

--наивная реализация, без аккумулятора
