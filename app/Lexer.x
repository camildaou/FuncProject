{
module Lexer (
    Alex,
    alexMonadScan,
    runAlex,
    Token(..),
    AlexPosn,
    scanTokens,
    showPosn) where

import Control.Arrow ((&&&))
import Data.Char
import Data.Functor
import Numeric (readHex)
}

%wrapper "monadUserState"
%token "Token AlexPosn"

$digit = 0-9
$hex = [0-9a-fA-F]

@number = \-?([1-9]$digit*|0)(\.$digit+)?([eE][\+\-]?$digit+)?

tokens :-
  <0>       $white+         { skip                 }
  <0>       "{"             { symbol LBrace        }
  <0>       "}"             { symbol RBrace        }
  <0>       "["             { symbol LBracket      }
  <0>       "]"             { symbol RBracket      }
  <0>       ":"             { symbol Colon         }
  <0>       ","             { symbol Comma         }
  <0>       "true"          { mkBoolLit True       }
  <0>       "false"         { mkBoolLit False      }
  <0>       "null"          { mkNullLit            }
  <0>       @number         { digit                }
  <0>       \"              { begin string         }
  <string>  \\b             { appendChar '\b'      }
  <string>  \\f             { appendChar '\f'      }
  <string>  \\n             { appendChar '\n'      }
  <string>  \\r             { appendChar '\r'      }
  <string>  \\t             { appendChar '\t'      }
  <string>  \\\\            { appendChar '\\'      }
  <string>  \\ \/           { appendChar '/'       }
  <string>  \\ \"           { appendChar '"'       }
  <string>  \\u $hex{4}     { unicodeChar          }
  <string>  [^\"]           { strChar              }
  <string>  \"              { emitStr `andBegin` 0 }

{
data Token a = LBrace    { tokLoc :: a }
             | RBrace    { tokLoc :: a }
             | LBracket  { tokLoc :: a }
             | RBracket  { tokLoc :: a }
             | Colon     { tokLoc :: a }
             | Comma     { tokLoc :: a }
             | StringLit { tokLoc :: a, tokStr :: String }
             | NumLit    { tokLoc :: a, tokNum :: Float }
             | BoolLit   { tokLoc :: a, tokBool :: Bool }
             | NullLit   { tokLoc :: a }
             | EOF       { tokLoc :: a }

surround = ("\"" ++) . (++ "\"")

instance Show (Token a) where
  show (LBrace _) = surround "{"
  show (RBrace _) = surround "}"
  show (LBracket _) = surround "["
  show (RBracket _) = surround "]"
  show (Colon _) = surround ":"
  show (Comma _) = surround ","
  show (StringLit _ s) = surround s
  show (NumLit _ n) = surround . show $ n
  show (BoolLit _ True) = "True"
  show (BoolLit _ False) = "False"
  show (NullLit _) = "null"
  show (EOF _) = "EOF"

showPosn (AlexPn _ l c) = show l ++ ":" ++ show c

mkL :: (AlexPosn -> String -> Token AlexPosn) -> AlexInput -> Int -> Alex (Token AlexPosn)
mkL tokfn (pos, _, _, input) len = return (tokfn pos $ take len input)

mkBoolLit :: Bool -> AlexInput -> Int -> Alex (Token AlexPosn)
mkBoolLit b = mkL (\p _ -> BoolLit p b)

mkNullLit :: AlexInput -> Int -> Alex (Token AlexPosn)        -- Added helper for null
mkNullLit = mkL (\p _ -> NullLit p)

symbol :: (AlexPosn -> Token AlexPosn) -> AlexInput -> Int -> Alex (Token AlexPosn)
symbol s = mkL (\p _ -> s p)

digit :: AlexInput -> Int -> Alex (Token AlexPosn)
digit = mkL (\p s -> NumLit p $ read s)

addChar c = alexGetUserState >>= alexSetUserState . (c:) >> alexMonadScan

appendChar c _ _ = addChar c
strChar (_, _, _, input) _ = addChar $ head input

unicodeChar (_, _, _, input) _ = case readHex $ drop 2 input of
  [(value, _)] -> addChar $ chr value
  _ -> alexError "invalid escape sequence"

emitStr :: AlexAction (Token AlexPosn)
emitStr (pos, _, _, _) _ = do
  strAcc <- alexGetUserState <&> reverse
  alexSetUserState ""
  return $ StringLit pos strAcc

get_pos :: Alex AlexPosn
get_pos = Alex (Right . (id &&& alex_pos))

alexEOF = do
  pos <- get_pos
  return $ EOF pos

type AlexUserState = String
alexInitUserState = ""

-- loop :: Alex [Token AlexPosn]
loop = do
  tok' <- alexMonadScan
  case tok' of
    EOF _ -> return []
    t -> (t :) <$> loop

scanTokens :: String -> Either String [Token AlexPosn]
scanTokens = flip runAlex loop
}