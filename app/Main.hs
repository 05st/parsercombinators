module Main where

import Parser

main :: IO ()
main = do
    input <- getLine
    print (pChar input)
