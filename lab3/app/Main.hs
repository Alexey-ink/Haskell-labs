module Main (main) where

import Lib
import Text.Read(readMaybe)
import Codec.Picture
import qualified Data.Vector.Unboxed as VU

getCaesarShift :: IO Int
getCaesarShift = do
    putStrLn "Введите значение сдвига для шифра Цезаря:"
    caesarShift <- getLine
    case readMaybe caesarShift of
        Just value -> return value
        Nothing -> do
            putStrLn "Ошибка: введите корректное целое число!"
            getCaesarShift


getNumberBits :: IO Int
getNumberBits = do
    putStrLn "Введите количество бит для кодирования:"
    inpNumberBits <- getLine
    case readMaybe inpNumberBits of
        Just value -> return value
        Nothing -> do
            putStrLn "Ошибка: введите корректное целое число!"
            getNumberBits


main :: IO ()
main = do
    caesarShift <- getCaesarShift
    numberBits <- getNumberBits
    
    let sourceTextPath = "src/bio.txt"
    let sourceImagePath = "src/sofya.bmp"
    let encodedImagePath = "src/sofya-" ++ show numberBits ++ "-" ++ show caesarShift ++ ".bmp"
    let decodedTextPath = "src/decoded-bio.txt"

    putStrLn $ "\nЧитаем текст из файла по пути: "++ "\"" ++ sourceTextPath ++ "\""
    inputText <- readFile sourceTextPath
    putStrLn $ "Считанный текст: " ++ take 66 inputText ++ "..."

    putStrLn "\n-------------------------Шифрование текста--------------------------\n"

    let alphabet = createAlphabet
    putStrLn $ "Размер алфавита равен " ++ show (length alphabet)

    let encryptedText = encryptCaesar alphabet caesarShift inputText
    putStrLn $ "Полученный шифр: \"" ++ take 50 encryptedText ++ "\""

    let encryptedTextBits = textToBits encryptedText
    putStrLn $ "Шифр в двоичном представлении: \"" ++ show (take 30 $ VU.toList encryptedTextBits) ++ "\""

    putStrLn "\n-------------------Кодирование текста в изображение-------------------\n"
    image <- readImage sourceImagePath
    case image of
        Left err -> putStrLn $ "Ошибка при чтении изображения: " ++ err
        Right dynImg -> do
            let img = convertRGB8 dynImg
            let width = imageWidth img
            let height = imageHeight img
            putStrLn $ "Ширина: " ++ show width ++ " Высота: " ++ show height ++ " пикселей"
            let totalBits = width * height * 3 * numberBits
            let bits = encryptedTextBits VU.++ VU.replicate (totalBits - VU.length encryptedTextBits) 0

            let resultImage = generateImage (encodePixel numberBits img bits) width height
            saveBmpImage encodedImagePath (ImageRGB8 resultImage)
            putStrLn $ "Изображение сохранено по пути: \"" ++ encodedImagePath ++ "\"" 


    putStrLn "\n-------------------Декодирование текста из изображения-------------------\n"
    case extractShift encodedImagePath of
        Just extractedCaesarShift -> do
            putStrLn $ "Из названия файла извлечен сдвиг: " ++ show extractedCaesarShift

            readEncodedImageResult <- readImage encodedImagePath
            case readEncodedImageResult of
                Left err -> putStrLn $ "Ошибка при чтении изображения: " ++ err
                Right dynImg -> do
                    let img = convertRGB8 dynImg
                    let bits = VU.fromList $ extractBitsFromImage numberBits img
                    putStrLn $ "Шифр в двоичном представлении: \"" ++ show (take 10 $ VU.toList bits) ++ "\""
                    let encryptedTextFromImage = takeWhile (/= '\NUL') (bitsToText bits)
                    putStrLn $ "Шифр: \"" ++ take 50 encryptedTextFromImage ++ "\""
                    let decryptedText = decryptCaesar alphabet extractedCaesarShift encryptedTextFromImage
                    putStrLn $ "Дешифрованный текст: \"" ++ take 66 decryptedText ++ "\""
                    writeFile decodedTextPath decryptedText
                    putStrLn $ "Текст сохранён по пути: \"" ++ decodedTextPath ++ "\"\n" 
        
        Nothing -> putStrLn "Не удалось извлечь ключ."




