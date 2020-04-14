checkString :: [Char]->Bool
helper :: [Char]->Int->Int->Int->Bool

checkString string = helper string 0 0 0

helper [] br1  br2 br3 = if((br1 == 0) && (br2 == 0) && (br3 == 0)) then True else False

helper (x:xs) br1 br2 br3                |x == '(' = helper xs (br1 + 1) br2 br3
                                         |x == ')' = helper xs (br1 - 1) br2 br3
                                         |x == '[' = helper xs br1 (br2 + 1) br3
                                         |x == ']' = helper xs br1 (br2 - 1) br3
                                         |x == '{' = helper xs br1 br2 (br3 + 1)
                                         |x == '}' = helper xs br1 br2 (br3 - 1)
                                         |otherwise = helper xs br1 br2 br3 
