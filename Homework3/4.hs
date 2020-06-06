checkString :: [Char]->Bool
helper :: [Char]->[Char]->Bool


checkString string = helper string [] 

helper []  buf = if buf == [] then True else False

helper (x:xs) buf             |x == '(' = helper xs (x:buf)
                              |x == ')' = if (head buf /= '(') then False else helper xs $tail buf
                              |x == '[' = helper xs (x:buf)
                              |x == ']' = if (head buf /= '[') then False else helper xs $tail buf
                              |x == '{' = helper xs (x:buf)
                              |x == '}' = if (head buf /= '{') then False else helper xs $tail buf
                              |otherwise = helper xs buf 

