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
    , stringSymbol
    , escaped
    , escapedString
    , string
    , StringConcat(..)
    , stringConcat
    , Declaration(..)
    , declaration
    , HttpGet(..)
    , httpGet
    , Method(..)
    , method
    , ErrorHandler(..)
    , errorHandler
    , HandleError(..)
    , handleError
    , Action(..)
    , action
    , ActionOrDeclaration(..)
    , Actions(..)
    , actions
    , WithCameraId(..)
    , withCameraId
    , WithLiveUnitId(..)
    , withLiveUnitId
    , InitFunc(..)
    , initFunc
    ) where

import           Data.Attoparsec.ByteString       hiding (string)
import           Data.Attoparsec.ByteString.Char8 (letter_ascii, digit, skipSpace)
import qualified Data.Attoparsec.ByteString.Char8 as ABC
-- import qualified Data.ByteString.Char8            as BSC
import           Text.RawString.QQ

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- <letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
letter :: Parser Char
letter = letter_ascii <?> "letter not found"

-- <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
-- <digits> ::= <digit> | <digit><digits>
digits :: Parser Word
digits = ABC.decimal

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
  = Assignment !Ident
  | NoAssignment
  deriving (Show, Eq)

assignment :: Parser Assignment
assignment = ( choice [ (Assignment <$> ident) <* (skipSpace <* ABC.string "<-" <* skipSpace)
                      , pure NoAssignment
                      ]
             ) <?> "Assignment malformed"


-- <stringsymbol> ::= " ", "!", "#", "$", "%", "&", "\"", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "`", "{", "|", "}", "~"
stringSymbol :: Parser Char
stringSymbol = ABC.satisfy isSymbolString <?> "symbol_string"

isSymbolString :: Char -> Bool
isSymbolString c =
     (c >= ' ' && c <= '!') -- Skip the " char
  || (c >= '#' && c <= '/')
  || (c >= ':' && c <= '@')
  || (c == '[')
  || (c >= ']' && c <= '`')
  || (c >= '{' && c <= '~')

escaped :: Parser Char
escaped = choice [ (ABC.string [r|\"|]) *> pure '\"'
                 , (ABC.string [r|\\|]) *> pure '\\'
                 , (ABC.string [r|\n|]) *> pure '\n'
                 ]

-- <escapedString> ::= <letter> <escapedString> | <digit> <escapedString> | <escaped> <escapedString> | <stringsymbol> <escapedString> | ""
escapedString :: Parser [Char]
escapedString = many' $ choice [ escaped, digit, letter, stringSymbol ]

-- <string> ::= """ <escapedString> """
string :: Parser [Char] -- TODO: escapedString cant end with '\\'
string = ABC.char '"' *> escapedString <* ABC.char '"'

-- <stringconcat> ::= <ident> | <string> | <string> "++" <stringconcat> | <ident> "++" <stringconcat>
data StringConcat
  = StrIdentConcat !Ident !StringConcat
  | StrConcat !String !StringConcat
  | StrIdent !Ident
  | StrString !String
  deriving (Show, Eq)

stringConcat :: Parser StringConcat
stringConcat = choice [ StrIdentConcat <$> ident <*> (skipSpace *> ABC.string "++" *> skipSpace *> stringConcat)
                      , StrConcat <$> string <*> (skipSpace *> ABC.string "++" *> skipSpace *> stringConcat)
                      , StrIdent <$> ident
                      , StrString <$> string
                      ]

-- <declaration> ::= <ident> "=" <stringconcat> ";"
data Declaration
  = Declaration !Ident !StringConcat
  deriving (Show, Eq)

declaration :: Parser Declaration
declaration =
  Declaration <$> ident <*> (skipSpace *> ABC.string "=" *> skipSpace *> stringConcat <* skipSpace <* ABC.string ";")

-- <httpGet> ::= "httpGet" <stringconcat>
newtype HttpGet
  = HttpGet StringConcat deriving (Show, Eq)

httpGet :: Parser HttpGet
httpGet = HttpGet <$> (ABC.string "httpGet" *> skipSpace *> stringConcat <* skipSpace)

-- <method> ::= <cameracolumn> | <liveunitcolumn> | <parseJson> | <httpGet> | <httpPost>
data Method
  = MethodHttpGet !HttpGet
  deriving (Show, Eq)

method :: Parser Method
method = choice [ MethodHttpGet <$> httpGet
                ]
         
-- <errorHandler> ::= "log" <stringconcat> | "email" <stringconcat>
data ErrorHandler
  = ErrorHandlerLog StringConcat
  | ErrorHandlerEmail StringConcat
  deriving (Show, Eq)

-- | how should an error be handled
errorHandler :: Parser ErrorHandler
errorHandler =
  choice [ ErrorHandlerLog <$> (ABC.string "log" *> skipSpace *> stringConcat <* skipSpace)
         , ErrorHandlerEmail <$> (ABC.string "email" *> skipSpace *> stringConcat <* skipSpace)
         ]

-- <handleError> ::= ": handleError" <digits> <errorHanlder> <error> | ""
type StatusCode = Word
data HandleError
  = HandleError StatusCode ErrorHandler
  deriving (Show, Eq)


-- | should the error be handled or not? and what status code should it have
handleError :: Parser [ HandleError ]
handleError = many' $ HandleError <$> (ABC.string ": handleError" *> skipSpace *> digits <* skipSpace) <*> errorHandler


-- <action> ::= <assignment> <method> <handleError> ";"
data Action
  = Action { actionAssignment :: Assignment
           , actionMethod :: Method
           , actionHandleError :: [ HandleError ]
           } deriving (Show, Eq)

action :: Parser Action
action = (Action <$> assignment <*> (skipSpace *> method) <*> (skipSpace *> handleError))
  <* skipSpace
  <* ABC.string ";"
  <* skipSpace

-- <actions> ::= <declaration> <actions> | <action> <actions> | <action> | <declaration>
data ActionOrDeclaration
  = IsAction Action
  | IsDeclaration Declaration
  deriving (Show, Eq)

newtype Actions
  = Actions [ ActionOrDeclaration ]
  deriving (Show, Eq)

actions :: Parser Actions
actions = Actions <$> (many' $ choice [ IsAction <$> action <* skipSpace
                                      , IsDeclaration <$> declaration <* skipSpace
                                      ])

-- <withCameraId> ::= "withCameraId" <ident>
newtype WithCameraId
  = WithCameraId Ident
  deriving (Show, Eq)

withCameraId :: Parser WithCameraId
withCameraId = WithCameraId <$> (ABC.string "withCameraId" *> skipSpace *> ident)

-- <withLiveUnitId> ::= "withLiveUnitId" <ident>
newtype WithLiveUnitId
  = WithLiveUnitId Ident
  deriving (Show, Eq)

withLiveUnitId :: Parser WithLiveUnitId
withLiveUnitId = WithLiveUnitId <$> (ABC.string "withLiveUnitId" *> skipSpace *> ident)

-- <initFunc> ::= <withCameraId> | <withLiveUnitId>
data InitFunc
  = InitWithCameraId !WithCameraId
  | InitWithLiveUnitId !WithLiveUnitId
  deriving (Show, Eq)

initFunc :: Parser InitFunc
initFunc = choice [ InitWithCameraId <$> withCameraId
                  , InitWithLiveUnitId <$> withLiveUnitId
                  ]

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
<httpPost> ::= "httpPost" <stringconcat> <postparams>




<responseJson> ::= "responseJson"
<jsonvalue> ::= <ident> | <string>
<jsonparam> ::= <string> ":" <jsonvalue>
<jsonparamlist> ::= <jsonparam> | <jsonparam> "," <jsonparamlist>
<jsonObject> ::= "{" <jsonparamlist> "}"
<response> ::= <responseJson> <digits> <jsonObject>
<systemCall> ::= <initFunc> "{" <actions> "}" <response> <error>
-}


{-

withCameraId camId {
  cameraPublicIp <- cameraColumn "publicIP" camId      : handleError 99 log "couldn't find camera public ip";
  cameraPublicPort <- cameraColumn "public_port" camId : handleError 98 log "couln'dt find camera public port";
  url = "http://" ++ cameraPublicIp ++ ":" ++ cameraPublicPort ++ "/my/endpoing";
  resp <- httpGet url                                          : handleError 100 log "request to blah sucked";
  myValue <- parseJson ["the","path","to","value"]             : handleError 101 log "parsing value sucked";
  httpPost url [ ("my", myValue), ("camera", cameraPublicIp) ] : handleError 102 log "posting to place sucked";
} responseJson 0 { "myValue" : myValue, "url" : url } : handlerError 103 log "the whole thing just sucked"

-}
