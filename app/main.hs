-- Main.hs

module Main where

import Control.Monad
import System.Environment (getArgs)
import System.IO (readFile)
import qualified Lexer as L
import qualified Parser as P
import Utils (getNestedField, parseFieldPath)
import Parser (JSON(..))
import Data.List (lookup)
import Data.List (intercalate)

-- | Pure function to parse JSON from a string
prettyParse :: String -> Either String JSON
prettyParse input = P.runParse P.jValue input

-- | Helper function to extract the plain value from JSON without type constructors
getPlainValue :: JSON -> String
getPlainValue (JString s) = show s
getPlainValue (JNumber n) = show n
getPlainValue (JBool b)   = show b
getPlainValue JNull       = "null"
getPlainValue (JArray a)  = "[" ++ intercalate ", " (map getPlainValue a) ++ "]"
getPlainValue (JObject o) = "{ " ++ intercalate ", " (map formatPair o) ++ " }"
  where
    formatPair (k, v) = k ++ ": " ++ getPlainValue v
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, fieldPath] -> do
            contents <- readFile filePath
            -- Remove all newline characters to treat as a single line
            let singleLine = concat (lines contents)
            case prettyParse singleLine of
                Right parsed -> do
                    case fieldPath of
                        "ALLFIELDS"  -> printAllFields parsed
                        "FIELDNAMES" -> printFieldNames parsed
                        _            -> do
                            -- Parse the field path
                            let fields = parseFieldPath fieldPath
                            
                            -- Access the specified field
                            case getNestedField fields parsed of
                                Just value -> putStrLn $ getPlainValue value
                                Nothing    -> putStrLn $ "Field \"" ++ fieldPath ++ "\" not found."
                Left err -> putStrLn $ "Parse error: " ++ err
        _ -> putStrLn "Usage: json-parser <file.json> <field.path | ALLFIELDS | FIELDNAMES>"

-- | Function to print all fields (key-value pairs) of a JObject, including nested fields
printAllFields :: JSON -> IO ()
printAllFields expr = traverseAndPrint "" expr
  where
    traverseAndPrint :: String -> JSON -> IO ()
    traverseAndPrint currentPath (JObject pairs) = mapM_ (processPair currentPath) pairs
    traverseAndPrint _ _ = putStrLn "The provided JSON is not an object or has no fields."

    processPair :: String -> (String, JSON) -> IO ()
    processPair currentPath (key, value) = do
        let newPath = if null currentPath then key else currentPath ++ "." ++ key
        case value of
            JObject pairs -> traverseAndPrint newPath (JObject pairs)
            _            -> putStrLn $ newPath ++ ": " ++ getPlainValue value

-- | Function to print only the field names of a JObject, including nested fields
printFieldNames :: JSON -> IO ()
printFieldNames expr = traverseAndCollect "" expr >>= mapM_ putStrLn
  where
    traverseAndCollect :: String -> JSON -> IO [String]
    traverseAndCollect currentPath (JObject pairs) = fmap concat $ mapM (processPair currentPath) pairs
    traverseAndCollect _ _ = return []
    
    processPair :: String -> (String, JSON) -> IO [String]
    processPair currentPath (key, value) = do
        let newPath = if null currentPath then key else currentPath ++ "." ++ key
        case value of
            JObject pairs -> traverseAndCollect newPath (JObject pairs)
            _            -> return [newPath]
