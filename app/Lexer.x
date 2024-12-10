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
  show (EOF _) = "EOF"
}