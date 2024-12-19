module Main (main) where
import Lib
import Text.Read(readMaybe)

menu :: IO Int
menu = putStrLn "Выберите действие: \n1. Составить модель из example.txt \n2. Организовать диалог моделей N-грамм.\nВведите номер пункта:" >>
       getLine >>= \input -> 
        case readMaybe input :: Maybe Int of
        Just choice | choice >= 1 && choice <= 2 -> return choice
        _ -> putStrLn "Ошибка! Введите число от 1 до 2." >> menu 


submenu :: IO FilePath 
submenu = 
    putStrLn "1.Пьеса \"Гроза\"\n2.Пьеса \"Бесприданница\"\nВведите номер пункта:" >>
    getLine >>= \input -> 
        case readMaybe input :: Maybe Int of
        Just 1 -> return "text1.txt"
        Just 2 -> return "text2.txt"
        _ -> putStrLn "Ошибка! Введите 1 или 2!" >> submenu

main :: IO ()
main =
    menu >>= \choice -> 
    case choice of 
        1 -> 
            readFile "example.txt" >>= \content ->
            let sentences = splitText content 
                dict = buildDictionary sentences 
            in saveDictionary "dict.txt" dict >>
            main
        2 -> 
            putStrLn "Выберите первое произведение: " >>
            submenu >>= \path1 -> 
            readFile path1 >>= \content1 -> 
            let sentences1 = splitText content1    
                dict1 = buildDictionary sentences1
            in saveDictionary "dict1.txt" dict1 >>
                putStrLn "Введите слово или пару слов для генерации фразы:" >>
                getLine >>= \input -> 
                processInput dict1 input >>

            putStrLn "Выберите второе произведение: " >>
            submenu >>= \path2 -> 
            readFile path2 >>= \content2 -> 
            let sentences2 = splitText content2      
                dict2 = buildDictionary sentences2
            in saveDictionary "dict2.txt" dict2 >>
                putStrLn "Введите начальное слово или пару слов для старта диалога:" >>
                getLine >>= \input2 -> 
                putStrLn "Введите количество сообщений от каждой модели:" >>
                getLine >>= \ms -> 
                let m = read ms :: Int in
                twoModelsDialog dict1 dict2 input2 m
        _ -> putStrLn "Ошибка!"