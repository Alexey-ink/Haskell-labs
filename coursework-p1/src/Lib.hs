{-# LANGUAGE InstanceSigs #-}

module Lib (
    processExpression
) where

import Control.Applicative (Alternative(..))
import Data.Char (digitToInt, isDigit)


newtype Parser tok a =
    Parser { runParser :: [tok] -> Maybe ([tok], a)}

instance Functor (Parser tok) where
    fmap :: (a -> b) -> Parser tok a -> Parser tok b
    fmap g (Parser p) = Parser f where
        f xs = case p xs of 
            Nothing -> Nothing
            Just (cs, c) -> Just (cs, g c)

instance Applicative (Parser tok) where
    pure :: a -> Parser tok a
    pure x = Parser $ \toks -> Just (toks, x)

    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> Nothing
            Just (xs', g) -> case v xs' of
                Nothing -> Nothing
                Just (xs'', x) -> Just (xs'', g x) 

instance Alternative (Parser tok) where
    empty :: Parser tok a
    empty = Parser $ \_ -> Nothing

    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> v xs
            z -> z

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (c:cs) | pr c = Just (cs, c)
    f _             = Nothing

char :: Char -> Parser Char Char
char c = satisfy (== c)

digit :: Parser Char Char
digit = satisfy isDigit

skipSpaces :: Parser Char String
skipSpaces = many (char ' ')

number :: Parser Char Int
number = skipSpaces *> (strToInt <$> some digit)
    where 
        strToInt = foldl (\acc x -> acc * 10 + digitToInt x) 0

data Operation = Add | Sub | Mul | Div deriving Show

operationToString :: Operation -> String
operationToString op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/" 

operationToOperator :: Operation -> (Int -> Int -> Int)
operationToOperator op = case op of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> div 
    

operation :: Parser Char Operation
operation = skipSpaces *> (
        char '+' *> pure Add <|>
        char '-' *> pure Sub <|>
        char '*' *> pure Mul <|>
        char '/' *> pure Div
    )

expression :: Parser Char (Int, Operation, Int)
expression = (,,) <$> number <*> operation <*> number <* skipSpaces

calculateExpression :: (Int, Operation, Int) -> Int
calculateExpression (a, op, b) = (operationToOperator op) a b

processExpression :: String -> String
processExpression s = case runParser expression s of
    Nothing -> err
    Just (cs, (a, op, b)) -> case cs of
        [] -> 
            show a ++ " " ++ 
            operationToString op ++ " " ++ 
            show b ++ " = " ++ 
            show (calculateExpression (a, op, b)) ++ "\n"
        _ -> err
    where
        err = error $ "Не удалось прочитать выражение: \"" ++ s ++ "\""  