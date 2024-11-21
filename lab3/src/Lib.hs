module Lib
    ( createAlphabet,
    encryptCaesar,
    textToBits,
    encodePixel,
    extractShift,
    extractBitsFromImage,
    decryptCaesar,
    bitsToText
    ) where

import Codec.Picture
import Data.List(elemIndex)
import Data.Bits (testBit, shiftL, complement, (.|.), (.&.))
import Data.Char (ord, chr)
import Data.Word (Word8)
import Text.Read (readMaybe)
import qualified Data.Vector.Unboxed as VU

createSpecialChars :: [Char]
createSpecialChars = [' ', '!', '"', '(', ')', ',', '-','.', ':', ';', '?', '\'']

createAlphabet :: [Char]
createAlphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ createSpecialChars

indexOfSymbol :: [Char] -> Char -> Int
indexOfSymbol alphabet symbol = 
    case elemIndex symbol alphabet of
        Just idx -> idx
        Nothing -> error "Character not found in alphabet!!!"


encryptCaesar :: [Char] -> Int -> String -> String
encryptCaesar alphabet shiftCaesar text = map caesarChar text
    where 
        caesarChar c = alphabet !! ((indexOfSymbol alphabet c + shiftCaesar) `mod` length alphabet)


textToBits :: String -> VU.Vector Int
textToBits text = VU.fromList $ concatMap charToBits text

charToBits :: Char -> [Int]
charToBits c = [if testBit (ord c) i then 1 else 0 | i <- [7,6..0]]

bitsToInt :: VU.Vector Int -> Int
bitsToInt bits = 
    sum [bit * (2 ^ index) | (bit, index) <- zip (VU.toList bits) [len,(len - 1)..0]]
    where
        len = VU.length bits - 1


intToWord8 :: Int -> Word8
intToWord8 x = fromIntegral x

word8ToInt :: Word8 -> Int
word8ToInt x = fromIntegral x

createMask :: Int -> Int
createMask shift = shiftL (complement 0) shift .&. complement 0


encodePixel :: Int -> Image PixelRGB8 -> VU.Vector Int -> Int -> Int -> PixelRGB8
encodePixel bitsPerByte img bits x y = PixelRGB8 newR newG newB
    where
        width = imageWidth img

        index = x + y * width
        startPos = index * 3 * bitsPerByte
        pixelBits = VU.slice startPos (3 * bitsPerByte) bits -- получаем биты для 3 каналов (R,G,B) одного пикселя

        -- Преобразуем вектор бит в целое число типа Int
        bitsIntR = bitsToInt $ VU.slice 0 bitsPerByte pixelBits
        bitsIntG = bitsToInt $ VU.slice bitsPerByte bitsPerByte pixelBits
        bitsIntB = bitsToInt $ VU.slice (2 * bitsPerByte) bitsPerByte pixelBits

        mask = createMask bitsPerByte

        PixelRGB8 r g b = pixelAt img x y
        newR = intToWord8 $ ((word8ToInt r) .&. mask) .|. bitsIntR
        newG = intToWord8 $ ((word8ToInt g) .&. mask) .|. bitsIntG
        newB = intToWord8 $ ((word8ToInt b) .&. mask) .|. bitsIntB

extractBits :: Int -> Pixel8 -> [Int]
extractBits bitsPerByte pixelByte = 
    [ if testBit pixelByte i then 1 else 0 | i <- [bitsPerByte-1, bitsPerByte-2..0] ] 

extractBitsFromPixel :: Int -> PixelRGB8 -> [Int]
extractBitsFromPixel bitsPerByte (PixelRGB8 r g b) =
    let bitsR = extractBits bitsPerByte r
        bitsG = extractBits bitsPerByte g
        bitsB = extractBits bitsPerByte b
    in bitsR ++ bitsG ++ bitsB

--Извлекает биты из изображения и преобразует их в вектор 
extractBitsFromImage :: Int -> Image PixelRGB8 -> [Int]
extractBitsFromImage bitsPerByte img = 
    let width = imageWidth img
        height = imageHeight img
        pixels = [ pixelAt img x y | y <- [0..height - 1], x <- [0..width - 1] ]
    in concatMap (extractBitsFromPixel bitsPerByte) pixels

extractShift :: String -> Maybe Int
extractShift path = 
    let shift = takeWhile (`elem` ['0'..'9']) (reverse $ takeWhile (/= '-') (reverse path))
    in readMaybe shift

    
decryptCaesar :: [Char] -> Int -> String -> String
decryptCaesar alphabet shift = 
    encryptCaesar alphabet (alphabetLength - (shift `mod` alphabetLength)) 
    where
        alphabetLength = length alphabet


bitsToText :: VU.Vector Int -> String
bitsToText bits
    | VU.null bits = []
    | otherwise = (chr $ bitsToInt (VU.take 8 bits)) : bitsToText (VU.drop 8 bits)