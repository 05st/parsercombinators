module Main where

import Parser

main :: IO ()
main = do
    putStrLn "Input:"
    input <- getLine
    case runParser parsePerson input of
        Left err -> putStrLn err
        Right (_, person) -> print person
