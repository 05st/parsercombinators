module Parser where

type Error = String
type Parser a = String -> Either Error (String, a)

pChar :: Parser Char
pChar (c : rest) = Right (rest, c)