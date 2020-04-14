revL :: [a]->[a]
helper :: [a]->[a]->[a]

revL [] = []
revL list = helper [] list

helper y [] = y
helper y (x : xs) = helper (x : y) xs
