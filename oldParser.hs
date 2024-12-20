import Control.Applicative
import Data.Char (isDigit)
import Data.Char (isSpace)

-- The 'JSON' data type represents the possible structures and values of a JSON document.
--
-- Constructors:
-- 
-- * 'JObject' [(String, JSON)]
--     - Represents a JSON object, which is a collection of key-value pairs.
--     - Each key is a 'String', and the associated value is a 'JSON' type (allowing nesting).
--     - Example: { "key1": "value1", "key2": { "nestedKey": true } }
-- 
-- * 'JArray' [JSON]
--     - Represents a JSON array, which is an ordered list of JSON values.
--     - Example: [1, "text", null, { "key": "value" }]
--
-- Deriving:
-- 
-- * 'Show': Allows the 'JSON' type to be displayed as a string for debugging.
-- * 'Eq': Enables comparison of 'JSON' values for equality.


data JSON
    = JNull 
    | JBool Bool 
    | JString String             
    | JNumber Double 
    | JArray [JSON]
    | JObject [(String, JSON)]                                  
  deriving (Show, Eq)


--Implementation of a parser
--the parser takes a string and return whaterver we want to parse

-- Maybe is used in case the parser fails when the input does not conform the expected format
--runParser is the fct that defines what the parser does


newtype Parser a = Parser 
    { runParser :: String -> Maybe(String ,a)
    } 


instance Functor Parser where
    fmap f (Parser p) = 
        Parser $ \input -> do
            (input',x) <- p input
            Just (input', f x)


instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'' , f a)


instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =  Parser $ \input -> p1 input <|> p2 input


spanP :: (Char -> Bool) -> Parser String
spanP f = 
  Parser $ \input -> 
    let (token , rest) = span f input
     in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = 
  Parser $ \input -> do 
    (input' , xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)


-- jNumber :: Parser JSON
-- jNumber = f <$> notNull(spanP isDigit)
--   where f ds = JNumber $ read ds

jNumber :: Parser JSON
jNumber = f <$> notNull (spanP isDigit) <*> optionalFraction
  where
    optionalFraction = (charP '.' *> spanP isDigit) <|> pure ""
    f intPart fracPart 
      | null fracPart = JNumber $ read intPart
      | otherwise     = JNumber $ read (intPart ++ "." ++ fracPart)


jNullP :: Parser JSON
jNullP = (\_ -> JNull) <$> stringP "null"



charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys,x)
            | otherwise = Nothing
        f [] = Nothing 

stringP :: String -> Parser String
stringP = sequenceA . map charP



--bool is eiher true or false
--we will use alternative interface



jBoolP :: Parser JSON
jBoolP = f <$> (stringP "true" <|> stringP "false")
  where f "true" = JBool True
        f "false" = JBool False
        f _       = undefined


stringLiteral :: Parser String
stringLiteral = (charP '"' *> spanP (/= '"') <* charP '"')

jString :: Parser JSON -- we need to add espacing support.
-- making sure it starts and ends with a quote and string in middle
jString = JString <$> stringLiteral 

sepBy :: Parser a -> Parser b -> Parser [b]
-- many parse this until it fails and the : to concatenate the results
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
-- <|> to handle the case where the parser fails, example: empty



spaceChar :: Parser String
spaceChar = spanP isSpace

jArray :: Parser JSON
jArray = JArray <$> (charP '[' *> spaceChar *> elements <* spaceChar <* charP ']') -- starts and ends with [] and has elements in between
  where
    elements = sepBy (spaceChar *> charP ',' <* spaceChar) jValue -- elemts are separated by comma

jObject :: Parser JSON 
jObject =
  JObject <$>
  (charP '{' *> spaceChar *> sepBy (spaceChar *> charP ',' <* spaceChar) pair <* spaceChar <* charP '}')
  where
   pair = liftA2 (,) (stringLiteral <* spaceChar <* charP ':' <* spaceChar) jValue


parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName p = do
 input <- readFile fileName
 return (snd <$> runParser p input)


jValue :: Parser JSON
jValue = jNullP <|> jBoolP <|> jNumber <|> jString <|> jArray <|> jObject



