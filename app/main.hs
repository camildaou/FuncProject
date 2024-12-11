-- Main.hs

module Main where

import Control.Monad
import Data.List (intercalate, lookup)
import qualified Lexer as L
import Parser (JSON (..))
import qualified Parser as P
import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Utils (getNestedField, parseFieldPath)

-- | Pure function to parse JSON from a string
prettyParse :: String -> Either String JSON
prettyParse input = P.runParse P.jValue input

-- | Helper function to extract the plain value from JSON without type constructors
getPlainValue :: JSON -> String
getPlainValue (JString s) = show s
getPlainValue (JNumber n) = show n
getPlainValue (JBool b) = show b
getPlainValue JNull = "null"
getPlainValue (JArray a) = "[" ++ intercalate ", " (map getPlainValue a) ++ "]"
getPlainValue (JObject o) = "{ " ++ intercalate ", " (map formatPair o) ++ " }"
  where
    formatPair (k, v) = k ++ ": " ++ getPlainValue v

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, fieldPath] -> processFile filePath fieldPath Nothing
    [filePath, fieldPath, "-o", outputFile] -> processFile filePath fieldPath (Just outputFile)
    _ -> putStrLn "Usage: json-parser <file.json> <field.path | ALLFIELDS | FIELDNAMES> [-o output.txt]"

processFile :: FilePath -> String -> Maybe FilePath -> IO ()
processFile filePath fieldPath outputFile = do
  contents <- readFile filePath
  -- Remove all newline characters to treat as a single line
  let singleLine = concat (lines contents)
  case prettyParse singleLine of
    Right parsed -> do
      result <- case fieldPath of
        "ALLFIELDS" -> printAllFields parsed
        "FIELDNAMES" -> printFieldNames parsed
        _ -> do
          -- Parse the field path
          let fields = parseFieldPath fieldPath
          -- Access the specified field
          return $ case getNestedField fields parsed of
            Just value -> getPlainValue value
            Nothing -> "Field \"" ++ fieldPath ++ "\" not found."
      case outputFile of
        Just file -> writeFile file result
        Nothing -> putStrLn result
    Left err -> putStrLn $ "Parse error: " ++ err

-- | Function to print all fields (key-value pairs) of a JObject, including nested fields
printAllFields :: JSON -> IO String
printAllFields expr = traverseAndPrint "" expr
  where
    traverseAndPrint :: String -> JSON -> IO String
    traverseAndPrint currentPath (JObject pairs) = fmap concat $ mapM (processPair currentPath) pairs
    traverseAndPrint _ _ = return "The provided JSON is not an object or has no fields.\n"

    processPair :: String -> (String, JSON) -> IO String
    processPair currentPath (key, value) = do
      let newPath = if null currentPath then key else currentPath ++ "." ++ key
      case value of
        JObject pairs -> traverseAndPrint newPath (JObject pairs)
        _ -> return $ newPath ++ ": " ++ getPlainValue value ++ "\n"

-- | Function to print only the field names of a JObject, including nested fields
printFieldNames :: JSON -> IO String
printFieldNames expr = fmap unlines $ traverseAndCollect "" expr
  where
    traverseAndCollect :: String -> JSON -> IO [String]
    traverseAndCollect currentPath (JObject pairs) = fmap concat $ mapM (processPair currentPath) pairs
    traverseAndCollect _ _ = return []

    processPair :: String -> (String, JSON) -> IO [String]
    processPair currentPath (key, value) = do
      let newPath = if null currentPath then key else currentPath ++ "." ++ key
      case value of
        JObject pairs -> traverseAndCollect newPath (JObject pairs)
        _ -> return [newPath]