module Parser where

import Control.Applicative

type Error = String
newtype Parser a = Parser { runParser :: String -> Either Error (String, a) }

parseAnyChar :: Parser Char
parseAnyChar = Parser f
    where
        f (x : xs) = Right (xs, x)
        f [] = Left "Empty input"

parsePredicate' :: (Char -> Bool) -> String -> Parser Char
parsePredicate' predicate errMsg = Parser f
    where
        f input =
            case runParser parseAnyChar input of
                Left err -> Left err
                Right (restInput, parsedChar) ->
                    if predicate parsedChar
                        then Right (restInput, parsedChar)
                        else Left ("Expected '" ++ errMsg ++ "', got '" ++ [parsedChar] ++ "'")

instance Functor Parser where
    fmap f pa = Parser (\input -> do
        (restInput, a) <- runParser pa input
        return (restInput, f a))

instance Applicative Parser where
    pure a = Parser (\input -> Right (input, a))
    pf <*> pa = Parser (\input -> do
        (restInput, f) <- runParser pf input
        (restInput', a) <- runParser pa restInput
        return (restInput', f a))

instance Monad Parser where
 -- return = pure
    pa >>= pfb = Parser (\input -> do
        (restInput, a) <- runParser pa input
        runParser (pfb a) restInput)

parserError :: String -> Parser a
parserError = Parser . const . Left

parsePredicate :: (Char -> Bool) -> String -> Parser Char
parsePredicate predicate errMsg = do
    parsedChar <- parseAnyChar
    if predicate parsedChar
        then return parsedChar
        else parserError ("Expected '" ++ errMsg ++ "', got '" ++ [parsedChar] ++ "'")

parseLetterC :: Parser Char
parseLetterC = parsePredicate (== 'c') "letter c"

parseChar :: Char -> Parser Char
parseChar char = parsePredicate (== char) (show char)

parseString :: String -> Parser String
parseString = traverse parseChar

choice :: Parser a -> Parser a -> Parser a
choice pa pb = Parser (\input -> 
    case runParser pa input of
        Left err -> runParser pb input
        Right (restInput, a) -> Right (restInput, a))

instance Alternative Parser where
    empty = (Parser . const . Left) [] -- Empty error string
    (<|>) = choice

parseBatOrBall :: Parser String
parseBatOrBall = parseString "bat" <|> parseString "ball"

-- EXAMPLE
data Person = Person
    { name :: String
    , age :: Int
    , height :: Float
    }

instance Show Person where
    show person = "Name: " ++ name person ++ "\nAge: " ++ show (age person) ++ " years\nHeight: " ++ show (height person) ++ " cm"

parseOneOf :: [Char] -> Parser Char
parseOneOf chars = parsePredicate (`elem` chars) ("One of '" ++ chars ++ "'")

parseInt :: Parser Int
parseInt = read <$> some (parseOneOf ['0'..'9'])

parseFloat :: Parser Float
parseFloat = do
    a <- show <$> parseInt
    parseChar '.'
    b <- show <$> parseInt
    return (read $ a ++ ('.' : b))

parsePerson :: Parser Person
parsePerson = do
    name <- some (parseOneOf ['a'..'z'] <|> parseOneOf ['A'..'Z'])
    parseChar ','
    age <- parseInt
    parseChar ','
    height <- parseFloat
    return (Person { name = name, age = age, height = height })