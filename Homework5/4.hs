
data Expr = Var 
          | Num Int
          | Add Expr Expr
          | Minus Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          deriving (Show)

instance Eq Expr where -- для паттерн матча
  Var == Var = True
  (Num left) == (Num right) = left == right
  (Add l1 l2) == (Add r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (Minus l1 l2) == (Minus r1 r2) = l1 == r1 && l2 == r2
  (Mul l1 l2) == (Mul r1 r2) = l1 == r1 && l2 == r2 || l1 == r2 && l2 == r1
  (Div l1 l2) == (Div r1 r2) = l1 == r1 && l2 == r2
  (Neg left) == (Neg right) = left == right
  _ == _ = False 
          
          
derivative' :: Expr -> Expr
derivative' Var = Num 1
derivative' (Num _) = Num 0
derivative' (Neg p) = Neg (derivative p)
derivative' (Add p q) = Add (derivative p) (derivative q)
derivative' (Minus p q) = Minus (derivative p) (derivative q)
derivative' (Mul p q) = Add (Mul q $ derivative p) (Mul p $ derivative q)
derivative' (Div p q) = Div (Minus (Div (derivative p) q) (Mul p $ derivative q))  (Mul q q)
    


simplify :: Expr -> Expr 
simplify expression = case expression of
        Var -> Var
        Num x -> Num x
        Add p q -> simplify' (Add (simplify p) (simplify q))
        Minus p q -> simplify' (Minus(simplify p) (simplify q))
        Div p q -> simplify' (Div (simplify p) (simplify q))
        Mul p q -> simplify' (Mul(simplify p) (simplify q))
        Neg p -> simplify' (Neg (simplify p))


simplify' expression = case expression of

        Mul left (Num 1) -> simplify' left
        Mul (Num 1) right -> simplify' right
        Mul _ (Num 0) -> Num 0
        Mul (Num 0) _ -> Num 0
        Mul (Num x) (Num y) -> Num (x * y)
        Mul left right -> Mul left right

        Div top (Num 1) -> simplify' top
        Div (Num 0) _ -> Num 0
        Div (Num x) (Num y) -> Num (div x y)
        Div Var Var -> Num 1
        Div left right
                       | left == right -> Num 1
                       | otherwise     -> Div left right

        Add (Num 0) right -> simplify' right
        Add left (Num 0) -> simplify' left
        Add (Num x) (Num y) -> Num (x + y)
        Add left right
                       | left == right -> Mul (Num 2) left
                       | otherwise     -> Add left right 

        Minus left (Num 0) -> simplify' left
        Minus (Num 0) right -> Neg (simplify' right)
        Minus (Num x) (Num y) -> Num (x-y)
        Minus left right
                       | left == right -> Num 0
                       | otherwise     -> Minus left right

        Neg (Num x) -> Num ((-1) * x)
        Neg x -> Neg x
        Var -> Var
        Num x -> Num x


derivative :: Expr -> Expr
derivative = simplify.derivative'


 
example = derivative $ Mul Var (Add (Add (Num 0) (Num 5)) $ Div Var (Num 2))
                        -- x * ((0 + 5) + x/2)



