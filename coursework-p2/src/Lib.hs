module Lib (
    splitText,
    buildDictionary,
    saveDictionary,
    processInput,
    twoModelsDialog
) where

import Data.Char (isLetter, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (nub, tails)
import System.IO
import System.Random (StdGen, randomR, newStdGen) -- mkStdGen
import UnescapingPrint (ushow)


splitText :: String -> [[String]]
splitText text = filter (not . null) $ map (processSentence . words) (splitSentences text)
  where
    splitSentences :: String -> [String]
    splitSentences [] = []
    splitSentences s =
        let (sentence, rest) = break isSeparator s
            rest' = dropWhile isSeparator rest
        in if null sentence
           then splitSentences rest'
           else sentence : splitSentences rest'

    isSeparator :: Char -> Bool
    isSeparator c = c `elem` ".!?;:()"

    processSentence :: [String] -> [String]
    processSentence = filter (not . null) . map cleanWord

    cleanWord :: String -> String
    cleanWord = map toLower . filter isLetter

buildDictionary :: [[String]] -> Map String [String]
buildDictionary sentences =
    let bigrams   = [ (w1, w2) | s <- sentences, (w1:w2:_) <- tails s ]
        trigrams  = [ (w1, w2, w3) | s <- sentences, (w1:w2:w3:_) <- tails s ]
        singleKeys = foldr (\(w1, w2) acc -> Map.insertWith (++) w1 [w2] acc) Map.empty bigrams
        singleKeys' = foldr (\(w1, w2, w3) acc -> Map.insertWith (++) w1 [w2 ++ " " ++ w3] acc) singleKeys trigrams
        doubleKeys = foldr (\(w1, w2, w3) acc -> Map.insertWith (++) (w1 ++ " " ++ w2) [w3] acc) singleKeys' trigrams
        --combined = Map.unionWith (++) singleKeys' doubleKeys
    in Map.map nub doubleKeys

saveDictionary :: FilePath -> Map String [String] -> IO ()
saveDictionary filePath dict = withFile filePath WriteMode $ \h -> 
    mapM_ (\(k,v) -> hPutStrLn h $ ushow k ++ ": " ++ ushow v) (Map.toList dict)

generatePhrase :: Map String [String] -> String -> StdGen -> [String]
generatePhrase dict start initGenState =
    let (len, initGenState') = randomR (2,15 :: Int) initGenState
    in reverse $ gp start [] len initGenState'
    where
        gp :: String -> [String] -> Int -> StdGen -> [String]
        gp key acc n genState 
            | n <= 0 = acc
            | otherwise =
                case Map.lookup key dict of
                    Nothing -> acc
                    Just [] -> acc
                    Just vals ->
                        let (i, newGenState) = randomR (0, length vals - 1) genState
                            next = vals !! i
                        in
                            gp next (next:acc) (n - length (words next)) newGenState

processInput :: Map String [String] -> String -> IO ()
processInput dict input =
    if Map.member input dict then
        newStdGen >>= \gen ->
        putStrLn $ unwords $ generatePhrase dict input gen
    else
        putStrLn "Нет в словаре"

findKeyForResponse :: Map String [String] -> [String] -> Maybe String
findKeyForResponse dict ws =
    case dropWhile (\w -> Map.notMember w dict) (reverse ws) of
        []    -> Nothing
        (x:_) -> Just x

dialogStep :: Map String [String] -> [String] -> IO [String]
dialogStep dict prevPhrase =
    case findKeyForResponse dict (words $ unwords prevPhrase) of
        Nothing -> putStrLn "Нет в словаре" >> return []
        Just key ->
            newStdGen >>= \gen ->
            let p = generatePhrase dict key gen
            in putStrLn ("(" ++ key ++ ") " ++ unwords p) >> return p

twoModelsDialog :: Map String [String] -> Map String [String] -> String -> Int -> IO ()
twoModelsDialog dict1 dict2 start m =
    newStdGen >>= \gen ->
    let first = generatePhrase dict1 start gen
    in putStrLn ("Модель 1: (" ++ start ++ ") " ++ unwords first) >>
        loop dict1 dict2 first m
    where
        loop :: Map String [String] -> Map String [String] -> [String] -> Int -> IO ()
        loop _ _ _ 0 = return ()
        loop d1 d2 prev i =
            putStr "Модель 2: " >>
            dialogStep d2 prev >>= \resp ->
            if null resp then return () else
            putStr "Модель 1: " >>
            dialogStep d1 resp >>= \resp2 ->
            if null resp2 then return () else
            loop d1 d2 resp2 (i-1)