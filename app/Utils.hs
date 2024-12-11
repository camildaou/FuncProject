module Utils where

--TODO: change names for JsonExpr

import Parser (JsonExpr(..))
import Data.List (find)

-- | Function to get a field from a JsonObject
getField :: String -> JsonExpr a -> Maybe (JsonExpr a)
getField key (JsonObject _ p) = 
    case find (\(k, _) -> matchKey k) p of
        Just (_, v) -> Just v
        Nothing     -> Nothing
  where
    matchKey :: JsonExpr a -> Bool
    matchKey (JsonStr _ s) = s == key
    matchKey _             = False
getField _ _ = Nothing

-- | Function to access nested fields using dot notation
getNestedField :: [String] -> JsonExpr a -> Maybe (JsonExpr a)
getNestedField [] expr = Just expr
getNestedField (k:ks) expr = do
    field <- getField k expr
    getNestedField ks field

-- | Function to convert dot-separated field string to list
parseFieldPath :: String -> [String]
parseFieldPath path = case break (== '.') path of
    (segment, "")     -> [segment]
    (segment, _:rest) -> segment : parseFieldPath rest