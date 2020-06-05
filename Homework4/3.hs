data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

instance Foldable Tree where
    foldr f list Empty = list
    foldr f list (Leaf a) = f a list
    foldr f list (Node left parent right) = foldr f (f parent (foldr f list left)) right

makeNodeList :: Tree a -> [a]
makeNodeList tree = foldr (:) [] tree


