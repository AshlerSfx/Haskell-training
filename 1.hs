import Data.List


data Polynom = Polynom[Int]

instance Num Polynom where
        signum = error "oops"
        fromInteger = error "oops"
        (+) (Polynom p) (Polynom q) = Polynom (customZipWith (+) p q)
        (-) (Polynom p) (Polynom q) = Polynom (customZipWith (-) p q)
        (*) (Polynom p) (Polynom q) = Polynom (multiplePolynoms p q)
        abs (Polynom p) = Polynom (map abs p)
        negate (Polynom p) = Polynom (map negate p)
        
 
instance Show Polynom where
        show (Polynom []) = ""
        show (Polynom (x : xs))         | xs == [] = show x
                                        | x == 0 = show (Polynom xs)
                                        | all (== 0) xs && (x == 1) = "x" ++ deg xs
                                        | all (== 0) xs && (x /= 1) = show x ++ "x" ++ deg xs
                                        | x == 1 = "x" ++ deg xs ++ "+" ++ show (Polynom xs)
                                        | otherwise = show x ++ "x" ++ deg xs ++ "+" ++ show (Polynom xs)       
deg xs | length xs == 1 = ""
       | otherwise = "^" ++ show (length xs)       
      


multiplePolynoms []       _  = [0]
multiplePolynoms _       []  = [0]
multiplePolynoms xs (y : []) = map (* y) xs
multiplePolynoms (y : []) xs = map (* y) xs
multiplePolynoms (x : xs) ys = customZipWith (+) (map (* x) ys) (0 : (multiplePolynoms xs ys))

customZipWith op a b = ziphelper op a b [] where
        ziphelper op [] [] result = reverse result
        ziphelper op (x : xs) [] result = ziphelper op xs [] (x : result)
        ziphelper op [] (y : ys) result = ziphelper op [] ys (y : result)
        ziphelper op (x : xs) (y : ys) result = ziphelper op xs ys ((op x y) : result)  



