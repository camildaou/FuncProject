module Utils where

--TODO: change names for JSON

import Parser (JSON(..))
import Data.List (find)

-- | Function to get a field from a JObject
getField :: String -> JSON -> Maybe JSON
getField key (JObject p) = 
    case find (\(k, _) -> matchKey k) p of
        Just (_, v) -> Just v
        Nothing     -> Nothing
  where
    matchKey :: String -> Bool
    matchKey s = s == key
getField _ _ = Nothing

-- | Function to access nested fields using dot notation
getNestedField :: [String] -> JSON -> Maybe JSON
getNestedField [] expr = Just expr
getNestedField (k:ks) expr = do
    field <- getField k expr
    getNestedField ks field

-- | Function to convert dot-separated field string to list
parseFieldPath :: String -> [String]
parseFieldPath path = case break (== '.') path of
    (segment, "")     -> [segment]
    (segment, _:rest) -> segment : parseFieldPath rest