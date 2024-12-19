module Main (main) where

import Lib

main :: IO ()
main =
    putStrLn "Введите имя файла:" >>
    getLine >>= \fileName ->
    readFile fileName >>= \content ->
    let expressions = lines content in
    putStrLn $ concatMap processExpression expressions
