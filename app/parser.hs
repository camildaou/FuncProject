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


charP :: Char -> Parser Char
charP x = Parser f
    where
        f (y:ys)
            | y == x = Just (ys,x)
            | otherwise = Nothing
        f [] = Nothing 



