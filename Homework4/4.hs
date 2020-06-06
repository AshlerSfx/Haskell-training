import System.IO

data Person = Person{ 
                name :: String,
                number :: String
              }        deriving (Eq, Show)

main = inLoop []


inLoop :: [Person] -> IO ()
inLoop  phoneBook = do
    putStrLn "Введите комманду "
    com <- getLine
    case com of

        '0':_ -> return()
    
        '1':_ -> do
                putStrLn "Введите имя"
                newName <- getLine
                putStrLn "Введите номер"
                newNumber <- getLine
                inLoop $ Person{name = newName, number = newNumber} : phoneBook
                
        '2':_ -> do
                putStrLn "Найти телефон по имени "
                reqName <- getLine
                print $ findPhone phoneBook reqName
                inLoop phoneBook

        '3':_ -> do
                putStrLn "Найти имя по телефону "
                reqNumber <- getLine
                print $ findName phoneBook reqNumber
                inLoop phoneBook

        '4':_ -> do
                putStrLn "Сохранить текущие данные в файл"
                writeIntoFile "example.txt" phoneBook
                inLoop phoneBook

        '5':_ -> do
                putStrLn "Считать данные из файла"
                list <- readFile "example.txt" 
                inLoop $ writeFromFile (lines list) phoneBook

        _ -> do
                putStrLn "Неправильный ввод"
                inLoop phoneBook


findPhone :: [Person] -> String -> String
findPhone [] _ = "Nothing found"
findPhone (line:other) reqName = if name line == reqName then  number line 
                                                         else findPhone other reqName


findName :: [Person] -> String -> String
findName [] _ = "Nothing found"
findName (line:other) reqPhone = if number line == reqPhone then name line
                                                            else findName other reqPhone


writeFromFile :: [String] -> [Person] -> [Person]
writeFromFile [] phBook = phBook

writeFromFile (one:other) phBook =
        if (elem newLine phBook) then writeFromFile other phBook
                                 else writeFromFile other $ newLine : phBook

        where newLine = Person{ name = head $ words one, number = last $ words one}


writeIntoFile :: String -> [Person] -> IO()

writeIntoFile fileName phoneBook = do
        list <- openFile fileName WriteMode
        helper list phoneBook

helper _ [] = return()
helper file (Person name number : other) = do
                    hPutStrLn file (name ++ " " ++ number)
                    helper file other

