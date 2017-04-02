{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib
    ( someFunc
    , letter
    , digits
    , identSymbol
    , mixed
    , Ident(..)
    , ident
    , Assignment(..)
    , assignment
    ) where

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (letter_ascii, digit, skipSpace)
import qualified Data.Attoparsec.ByteString.Char8 as ABC

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
letter :: Parser Char
letter = letter_ascii <?> "letter not found"

-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
-- <digits> ::= <digit> | <digit><digits>
digits :: Parser [Char]
digits = many1 digit

-- <symbol> ::=  "-" | "_"
identSymbol :: Parser Char
identSymbol = ABC.satisfy isSymbolIdent <?> "symbol_ident"

isSymbolIdent :: Char -> Bool
isSymbolIdent c =
  (c == '-' || c == '_')

-- <mixed> ::= <letter><mixed> | <digit><mixed> | <symbol><mixed> | ""
mixed :: Parser [Char]
mixed = (many1 $ choice [ digit, letter, identSymbol ]) <?> "Looking for word"


-- <ident> ::= <letter><mixed>
newtype Ident = Ident [Char] deriving (Show, Eq)

ident :: Parser Ident
ident = ((Ident <$>) $ (:) <$> letter <*> mixed) <?> "Identifier couldn't be found"

-- <assignemnt> ::= <ident> "<-" | ""
data Assignment
  = Assignment Ident
  | NoAssignment
  deriving (Show, Eq)

assignment :: Parser Assignment
assignment = ( choice [ (Assignment <$> ident) <* (skipSpace <* ABC.string "<-")
                      , pure NoAssignment
                      ]
             ) <?> "Assignment malformed"


-- <stringsymbol> ::= TODO
identSymbol :: Parser Char
identSymbol = ABC.satisfy isSymbolIdent <?> "symbol_ident"

isSymbolIdent :: Char -> Bool
isSymbolIdent c =
  (c == '-' || c == '_')

-- <notquote> ::= <letter> <notquote> | <digit> <notquote> | "\"" <notquote> | <stringsymbol> <notquote> | ""
-- <string> ::= """ <notquote> """
-- <stringconcat> ::= <string> | <string> "++" <string>
-- <declaration> ::= <ident> "=" <stringconcat> ";"

-- <url> ::= <string> | <ident>
data Url
  = UrlHardcoded String
  | UrlVariable Ident

url :: Parser Url
url = error "do this"

-- <httpGet> ::= "httpGet" <url>
newtype HttpGet
  = HttpGet Url deriving (Show, Eq)

httpGet :: Parser HttpGet
httpGet = error "do this" -- ABC.string "httpGet" *> skipSpace *>  

-- <method> ::= <cameracolumn> | <liveunitcolumn> | <parseJson> | <httpGet> | <httpPost>

-- <action> ::= <assignment> <method> <error> ";"

{-
BNF DSL

<cameracolumn> ::= "cameraColumn" <string> <ident>
<liveunitcolumn> ::= "liveUnitColumn" <string> <ident>
<strings> ::= <string> | <string> "," <strings>
<stringArray> ::= "[" <strings> "]"
<parseJson> ::= "parseJson" <stringArray>
<postvalue> ::= <string> | <ident>
<postparam> ::= "(" <string> "," <postvalue> ")"
<postparamlist> ::= <postparam> | <postparam> "," <postparamlist>
<postparams> ::= "[" <postparamlist> "]"
<httpPost> ::= "httpPost" <url> <postparams>

<errorHandler> ::= "log" <string> | "email" <string>
<error> ::= ": handleError" <digits> <errorHanlder> | ""

-- <declarations> ::= <declaration> | <declaration><declarations>
<withCameraId> ::= "withCameraId" <ident>
<withLiveUnitId> ::= "withLiveUnitId" <ident>
<initFunc> ::= <withCameraId> | <withLiveUnitId>
<responseJson> ::= "responseJson"
<jsonvalue> ::= <ident> | <string>
<jsonparam> ::= <string> ":" <jsonvalue>
<jsonparamlist> ::= <jsonparam> | <jsonparam> "," <jsonparamlist>
<jsonObject> ::= "{" <jsonparamlist> "}"
<response> ::= <responseJson> <digits> <jsonObject>
<systemCall> ::= <initFunc> "{" <declarations> "}" <response>
-}


{-

withCameraId camId {
  cameraPublicIp <- cameraColumn "publicIP" camId      : handleError 99 log "couldn't find camera public ip";
  cameraPublicPort <- cameraColumn "public_port" camId : handleError 98 log "couln'dt find camera public port";
  url = "http://" ++ cameraPublicIp ++ ":" ++ cameraPublicPort ++ "/my/endpoing";
  resp <- httpGet url                                          : handleError 100 log "request to blah sucked";
  myValue <- parseJson ["the","path","to","value"]             : handleError 101 log "parsing value sucked";
  httpPost url [ ("my", myValue), ("camera", cameraPublicIp) ] : handleError 102 log "posting to place sucked";
} responseJson 0 { "myValue" : myValue, "url" : url }

-}
