
import Control.Monad.Zip

find :: Ord a => [a] -> Maybe a
find xs = (\list -> if (null list) then Nothing else Just (head list)) 


   (customzip3 (drop 1 xs) xs (drop 2 xs) >>= (\(a, b, c) -> if (a > b && a > c) then [a] 
                                                                                 else []))


customzip3 xs ys zs = mzipWith (\x (y, z) -> (x, y, z)) xs (mzip ys zs)
