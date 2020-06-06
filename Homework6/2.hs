import System.Random

data BST a = Null | Node  a (BST a) (BST a)
        deriving Show

--размер
size Null = 0
size (Node _ left right) = 1 + (size left) + (size right)

--высота
height Null = 0
height (Node _ left right) = 1 + (height left) `max` (height right)

--Поиск нужного нода
searchNode x Null = False
searchNode x (Node value left right) | x == value = True
                                     | x < value = searchNode x left
                                     | otherwise = searchNode x right
--добавление
insert x Null = Node x Null Null
insert x (Node value left right) | x == value = Node value left right
                                 | x > value = Node value left (insert x right)
                                 | x < value = Node value (insert x left) right 



-- удаление для крайних случаев и перемещение в корень наибольшего левого нода в остальных
remove' (Node _ Null  Null) = Null
remove' (Node _ left Null) = left
remove' (Node _ Null right) = right
remove' (Node _ left right) = Node (findMaxNode left) (deleteDuplicateOfNewRoot left) right

-- поиск максимума
findMaxNode (Node value _  Null) = value
findMaxNode (Node _ _ right) = findMaxNode right

-- перемещение нового корня при удалении
deleteDuplicateOfNewRoot (Node _ left Null) = left
deleteDuplicateOfNewRoot (Node value left right) = Node value left (deleteDuplicateOfNewRoot right)


--поиск на удаление
findToRemove x Null = Null
findToRemove x (Node value left right) | x == value = remove' (Node value left right)
                                       | x < value = Node value (findToRemove x left) right
                                       | otherwise = Node value left (findToRemove x right)
--Случайное заполнение
randomInsert Null = return Null

randomInsert (Node value left right) = do
        value <- randomIO (0,50)
        left <- randomInsert left
        right <- randomInsert right
        return (Node value left right)
        


