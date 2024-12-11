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
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM, filterM)
import Data.List (isSuffixOf)
import Data.Char (isSpace)

-- | Pure function to parse JSON from a string
prettyParse :: String -> Either String JSON
prettyParse input = P.runParse P.jValue input

-- | Helper function to extract the plain value from JSON without type constructors
getPlainValue :: JSON -> String
getPlainValue (JString s) = s
getPlainValue (JNumber n) = show n
getPlainValue (JBool b) = show b
getPlainValue JNull = "null"
getPlainValue (JArray a) = "[" ++ intercalate ", " (map getPlainValue a) ++ "]"
getPlainValue (JObject o) = "{ " ++ intercalate ", " (map formatPair o) ++ " }"
  where
    formatPair (k, v) = k ++ ": " ++ getPlainValue v


-- | Entry point of the JSON parser
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> putStrLn err
        Right options -> processOptions options

-- | Data type to hold command-line options
data Options = Options
    { inputPaths  :: [FilePath]           -- ^ List of input files or directories
    , fieldPath   :: String               -- ^ Field path to extract
    , outputFile  :: Maybe FilePath      -- ^ Optional output file
    }

-- | Parse command-line arguments
parseArgs :: [String] -> Either String Options
parseArgs args =
    case args of
        ["--help"] -> Left usage
        _ -> case extractOptions args of
                Just opts -> Right opts
                Nothing -> Left usage
  where
    usage = "Usage:\n\
            \  json-parser <file1.json> <file2.json> ... <fieldPath | ALLFIELDS | FIELDNAMES> [-o output.txt]\n\
            \  json-parser <directory> <fieldPath | ALLFIELDS | FIELDNAMES> [-o output.txt]\n"

    extractOptions :: [String] -> Maybe Options
    extractOptions xs =
        let (opts, rest) = span (/= "-o") xs
            output = case rest of
                        ("-o":out:_) -> Just out
                        _            -> Nothing
        in case reverse opts of
            (fp:fps) -> Just $ Options (reverse fps ++ [fp]) fp output
            _        -> Nothing

-- | Process the parsed options
processOptions :: Options -> IO ()
processOptions (Options paths fldPath mOut) = do
    jsonFiles <- concat <$> mapM resolvePath paths
    results <- forM jsonFiles $ \file -> do
        processFile file fldPath mOut
    case mOut of
        Just out -> writeFile out (concat results)
        Nothing  -> mapM_ putStrLn results

-- | Resolve input paths to a list of JSON files
resolvePath :: FilePath -> IO [FilePath]
resolvePath path = do
    exists <- doesDirectoryExist path
    if exists
        then listJsonFiles path
        else return [path | takeExtension path == ".json"]

-- | List all JSON files in a directory (non-recursive)
listJsonFiles :: FilePath -> IO [FilePath]
listJsonFiles dir = do
    contents <- listDirectory dir
    let jsons = filter (isSuffixOf ".json") contents
    return [dir </> file | file <- jsons]

-- | Process a single JSON file
processFile :: FilePath -> String -> Maybe FilePath -> IO String
processFile file fldPath mOut = do
    contents <- readFile file
    let singleLine = concat (lines contents)
    case prettyParse singleLine of
        Right parsed -> do
            result <- case fldPath of
                "ALLFIELDS"  -> printAllFields parsed
                "FIELDNAMES" -> printFieldNames parsed
                _            -> extractField fldPath parsed
            return ("Processing: " ++ file ++ "\n" ++ result ++ "\n")
        Left err -> return $ "Parse error in " ++ file ++ ": " ++ err ++ "\n"


-- | Extract a specific field value
extractField :: String -> JSON -> IO String
extractField fldPath parsed =
    let fields = parseFieldPath fldPath
        result = case getNestedField fields parsed of
            Just value -> getPlainValue value
            Nothing    -> "Field \"" ++ fldPath ++ "\" not found.\n"
    in return result

-- | The rest of your existing functions (getPlainValue, printAllFields, printFieldNames, prettyParse, etc.)
-- Ensure they are included here as per your current implementation.

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