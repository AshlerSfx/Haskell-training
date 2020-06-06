import System.IO

main = do
    inLoop []

addNewOne :: [Int] -> Int -> [Int]

addNewOne [] y = y : []

addNewOne (x : xs) y | (y < x) = y : x : xs
                     | otherwise = x : addNewOne xs y
                          
                         
remove :: [Int] -> Int -> [Int]
remove [] y = []
remove xs y | elem y xs == False = xs
            | otherwise = filter (\x -> (x /= y)) xs

inLoop :: [Int] -> IO ()
inLoop sortedList = do
    putStrLn "Введите команду"
    com <- getLine
    
    case com of
        '0':_ -> return()

        '1':value -> do
                putStrLn "add value to sorted list"
                inLoop (addNewOne sortedList (read value::Int))

        '2':value -> do
                putStrLn "remove value from list"
                inLoop (remove sortedList (read value::Int))

        '3':_ -> do
                putStrLn "add value to sorted list"
                print sortedList
                inLoop sortedList

        _ -> do
                putStrLn "Некорректный ввод"
                inLoop sortedList    


    
