
import Data.List
import Control.Monad.State


data Graph v = Graph [(Int,v)] [(Int,Int,Int)] deriving (Show)
data Cost v = Cost (v, Int) deriving (Show)


from (v1, v2, e)    = v1
to (v1, v2, e)      = v2
w (v1, v2, e)  = e

vertex (Cost a) = fst a
edge (Cost a) = snd a



getVal costList v2 = edge  (head $ filter ((== v2).vertex) costList)


--в начальную точку 0, остальным максимальная метка
costsAtStart first vertexList = [if x == first then Cost (x, 0) else Cost (x, 100000) | x <- map fst vertexList] 


--запуск
search (Graph vertexList edges) start = 
            evalState (searchInRecursion (Graph vertexList edges) $ costsAtStart start vertexList) (map fst vertexList)

--поиск, обновляем состояние пока дойдём до нулевого
searchInRecursion (Graph vertexList edges) costList = do {
        state <- get;
        if null $ state
                then return costList
                else do 
                        let v = minimumBy (\x y -> compare (getVal costList x) $ getVal costList y) state
                        put $ filter (/= v) state
                        searchInRecursion (Graph vertexList edges) $
                                                marksUpdate (filter (\t -> (v == (from t) || v == (to t))) edges) costList
        }


--новое состояние меток
marksUpdate [] costList = costList

marksUpdate (x : xs) costList   | ((getVal costList $ from x) + (w x) <= (getVal costList $ to x)) = 
                                     marksUpdate xs $ (Cost (to x, getVal costList (from x) + (w x))) : (filter ((/= (to x)).vertex) costList)

                                | ((getVal costList $ to x) + (w x) <= (getVal costList (from x))) = 
                                        marksUpdate xs $ (Cost (from x, getVal costList (to x) + (w x))) : (filter ((/= (from x)).vertex) costList)

                                | otherwise = marksUpdate xs costList





