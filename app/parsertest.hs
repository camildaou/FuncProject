module Parser (JSON(..), parseFile, runParse, jBoolP) where

import Lexer
import Control.Applicative
import Data.Maybe

data JSON
  = JNull
  | JBool Bool
  | JString String
  | JNumber Float
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: [Token AlexPosn] -> Maybe ([Token AlexPosn], a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \tokens -> do
    (rest, result) <- p tokens
    Just (rest, f result)

instance Applicative Parser where
  pure x = Parser $ \tokens -> Just (tokens, x)
  (Parser pf) <*> (Parser pa) = Parser $ \tokens -> do
    (tokens', f) <- pf tokens
    (tokens'', a) <- pa tokens'
    Just (tokens'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \tokens -> p1 tokens <|> p2 tokens

instance Monad Parser where
  return = pure
  (Parser pa) >>= f = Parser $ \tokens -> do
    (tokens', a) <- pa tokens
    runParser (f a) tokens'


matchToken :: (Token AlexPosn -> Maybe a) -> Parser a
matchToken test = Parser $ \tokens -> case tokens of
  (tok:rest) -> do
    let _ = print tok
    case test tok of
      Just value -> Just (rest, value)
      Nothing    -> Nothing
  [] -> Nothing


tokenP :: (Token AlexPosn -> Bool) -> Parser (Token AlexPosn)
tokenP test = matchToken (\tok -> if test tok then Just tok else Nothing)

symbolP :: Token AlexPosn -> Parser ()
symbolP expected = () <$ tokenP (== expected)

jNullP :: Parser JSON
jNullP = matchToken test
  where
  test (NullLit _) = Just JNull
  test _           = Nothing

jBoolP :: Parser JSON
jBoolP = matchToken test
  where
  test (BoolLit _ True)  = Just (JBool True)
  test (BoolLit _ False) = Just (JBool False)
  test _ = Nothing

jNumber :: Parser JSON
jNumber = matchToken input
  where
  input (NumLit _ n) = Just (JNumber n)
  input _            = Nothing

jString :: Parser JSON
jString = matchToken test
  where
  test (StringLit _ s) = Just (JString s)
  test _               = Nothing

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []


spaceP :: Parser ()
spaceP = () <$ many (tokenP isWhitespace)
  where
  isWhitespace (EOF _) = True
  isWhitespace _       = False

  
jArray :: Parser JSON
jArray = do
  symbolP (LBracket dummyPos)
  elements <- elementsP
  symbolP (RBracket dummyPos)
  let result = JArray elements
  -- Debug: Print the parsed array
  let _ = print ("Parsed array:", result)
  return result
  where
  elementsP = sepBy (symbolP (Comma dummyPos)) jValue <|> pure []

jObject :: Parser JSON
jObject = do
  symbolP (LBrace dummyPos)
  pairs <- pairsP
  symbolP (RBrace dummyPos)
  let result = JObject pairs
  -- Debug: Print the parsed object
  let _ = print ("Parsed object:", result)
  return result
  where
  pairsP = sepBy (symbolP (Comma dummyPos)) pair
  pair = do
    k <- key
    symbolP (Colon dummyPos)
    v <- jValue
    return (k, v)
  key = matchToken test
    where
    test (StringLit _ s) = Just s
    test _ = Nothing        

jValue :: Parser JSON
jValue = jNullP <|> jBoolP <|> jNumber <|> jString <|> jArray <|> jObject

parseFile :: FilePath -> Parser JSON -> IO (Either String JSON)
parseFile fileName parser = do
  input <- readFile fileName
  case scanTokens input of
    Left err -> return (Left err)
    Right tokens -> do
      print tokens  -- Debug: Print the list of tokens
      case runParser parser tokens of
        Just (_, result) -> return (Right result)
        Nothing -> return (Left "Parsing failed.")


dummyPos :: AlexPosn
dummyPos = AlexPn 0 0 0

runParse :: Parser a -> String -> Either String a
runParse parser input = case scanTokens input of
  Left err -> Left err
  Right tokens -> case runParser parser tokens of
    Just ([], result) -> Right result
    _ -> Left "Parsing failed."
