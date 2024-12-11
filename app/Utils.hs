module Utils where

--TODO: change names for JSON

import Parser (JSON(..))
import Data.List (find)
import Data.Char (isDigit)

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


getNestedField :: [String] -> JSON -> Maybe JSON
getNestedField [] json = Just json
getNestedField (key:keys) (JObject obj) =
    case lookup key obj of
        Just value -> getNestedField keys value
        Nothing -> Nothing
getNestedField (key:keys) (JArray arr)
    | all isDigit key =
        let index = read key :: Int
        in if index >= 0 && index < length arr
            then getNestedField keys (arr !! index)
            else Nothing
    | otherwise =
        let results = map (\item -> getNestedField (key:keys) item) arr
        in Just (JArray (map (\(Just x) -> x) (filter (/= Nothing) results)))
getNestedField _ _ = Nothing




-- | Function to convert dot-separated field string to list
parseFieldPath :: String -> [String]
parseFieldPath path = case break (== '.') path of
    (segment, "")     -> [segment]
    (segment, _:rest) -> segment : parseFieldPath rest
