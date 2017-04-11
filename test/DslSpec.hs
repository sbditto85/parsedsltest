module DslSpec where

import           Data.Attoparsec.ByteString  hiding (string)
import qualified Data.ByteString.Char8       as BSC
import qualified Data.Either                 as E
import qualified Data.List                   as L
import           Data.Semigroup              ((<>))
-- import qualified Debug.Trace                 as D
import           Lib
import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
-- import           Text.RawString.QQ

replaceSlash :: [Char] -> [Char]
replaceSlash ('\\':'\"':xs) = '\"' : replaceSlash xs
replaceSlash ('\\':'\\':xs) = '\\' : replaceSlash xs
replaceSlash ('\\':'n':xs) = '\n' : replaceSlash xs
replaceSlash (x:xs) = x : replaceSlash xs
replaceSlash [] = []

trimQuotes :: [Char] -> [Char]
trimQuotes = reverse . (L.drop 1) . reverse . (L.drop 1)

escapedCharToChar :: [Char] -> Char
escapedCharToChar "\\\"" = '\"'
escapedCharToChar "\\\\" = '\\'
escapedCharToChar "\\n" = '\n'
escapedCharToChar _ = '\0'

genLetter :: Gen Char
genLetter = elements (['A'..'Z'] ++ ['a'..'z'])

genDigit :: Gen Char
genDigit = elements ['0'..'9']

genDigits :: Gen String
genDigits = listOf1 genDigit

genIdentSymbol :: Gen Char
genIdentSymbol = elements ['-', '_']


genMixed :: Gen [Char]
genMixed = listOf1 $ oneof [ genDigit, genIdentSymbol, genLetter ]

genIdent :: Gen [Char]
genIdent = do
  f <- genLetter
  rest <- genMixed
  pure $ f : rest

genBadIdent :: Gen [Char]
genBadIdent = do
  f <- oneof [ genDigit, genIdentSymbol ]
  rest <- genMixed
  pure $ f : rest

genStringSymbol :: Gen Char
genStringSymbol = elements [' ', '!', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '_', '`', '{', '|', '}', '~']

genEscaped :: Gen [Char]
genEscaped = oneof [ pure "\\\\"
                   , pure "\\\""
                   , pure "\\n"
                   ]

genEscapedString :: Gen [Char]
genEscapedString =
  fmap mconcat $ listOf1 $ oneof [ listOf $ oneof [ genDigit, genLetter, genStringSymbol ]
                                 , genEscaped
                                 ]

genString :: Gen [Char]
genString =
  let
    begQuoteAndStr = (++) <$> (pure "\"") <*> genEscapedString
    lastChar = (:[]) <$> oneof [ genDigit, genLetter ]
    withLastChar = (++) <$> begQuoteAndStr <*> lastChar
  in
    (++) <$> withLastChar <*> (pure "\"")

genStringConcat :: Gen StringConcat
genStringConcat =
  oneof [ genStrIdentConcat
        , genStrConcat
        , genStrIdent
        , genStrString
        ]
  where
    genStrIdentConcat = 
      let
        ident' = Ident <$> genIdent
        strConcat = genStringConcat
      in
        StrIdentConcat <$> ident' <*> strConcat

    genStrConcat =
      let
        str = genString
        strConcat = genStringConcat
      in
        StrConcat <$> str <*> strConcat

    genStrIdent = StrIdent <$> (Ident <$> genIdent)
    genStrString = StrString <$> genString

stringConcatToString :: StringConcat -> String
stringConcatToString (StrIdentConcat (Ident ident') rest) = ident' ++ " ++ " ++ (stringConcatToString rest)
stringConcatToString (StrConcat str rest) = str ++ " ++ " ++ (stringConcatToString rest)
stringConcatToString (StrIdent (Ident ident')) = ident'
stringConcatToString (StrString str) = str

genStrConcatToParsed :: StringConcat -> StringConcat
genStrConcatToParsed (StrIdentConcat ident' rest) = StrIdentConcat ident' (genStrConcatToParsed rest)
genStrConcatToParsed (StrConcat str rest) = StrConcat (trimQuotes $ replaceSlash str) (genStrConcatToParsed rest)
genStrConcatToParsed strIdent@(StrIdent _) = strIdent
genStrConcatToParsed (StrString str) = StrString (trimQuotes $ replaceSlash str)

  
spec :: Spec
spec = do
  prop "letter parses out all letters" $ do
    forAll genLetter $ \x ->
      let
        res = parseOnly (letter <* endOfInput) $ BSC.singleton x
      in
        res == (Right x)

  prop "digits parses out digits" $ do
    forAll genDigits $ \x ->
      let
        res = parseOnly (digits <* endOfInput) $ BSC.pack x
      in
        res == (Right x)

  prop "identSymbol parses out of possible identSymbols" $ do
    forAll genIdentSymbol $ \x ->
      let
        res = parseOnly (identSymbol <* endOfInput) $ BSC.singleton x
      in
        res == (Right x)

  prop "mixed parsed out of the possibilities" $ do
    forAll genMixed $ \x ->
      let
        res = parseOnly (mixed <* endOfInput) $ BSC.pack x
      in
        res == (Right x)

  prop "ident parsed out of the possibilities" $ do
    forAll genIdent $ \x ->
      let
        res = parseOnly (ident <* endOfInput) $ BSC.pack x
      in
        res == (Right (Ident x))
  
  prop "ident NOT parsed out when doesn't start with letter" $ do
    forAll genBadIdent $ \x ->
      let
        res = parseOnly (ident <* endOfInput) $ BSC.pack x
      in
        E.isLeft res

  prop "assignment is parsed when there is an ident followed by <-" $ do
    forAll genIdent $ \x ->
      let
        res = parseOnly (assignment <* endOfInput) $ (BSC.pack x) <> " <-"
      in
        res == (Right $ Assignment (Ident x))
    
  it "assignemnt will parse out a NoAssigment on empty string" $
    let
      res = parseOnly (assignment <* endOfInput) $ mempty
    in
      res == (Right $ NoAssignment)

  prop "stringSymbol parsed out of possibilities" $ do
    forAll genStringSymbol $ \x ->
      let
        res = parseOnly (stringSymbol <* endOfInput) $ (BSC.singleton x)
      in
        res == (Right x)

  it "stringSymbol will NOT parse out a \" symbol" $
    let
      res = parseOnly (stringSymbol <* endOfInput) $ "\""
    in
      E.isLeft res

  prop "quote doesn't parse any string symbol" $ do
    forAll genStringSymbol $ \x ->
      let
        res = parseOnly (escaped <* endOfInput) $ (BSC.singleton x)
      in
        E.isLeft res
        
  prop "escaped parses the escaped symbols" $ do
    forAll genEscaped $ \str ->
      let
        res = parseOnly (escaped <* endOfInput) $ (BSC.pack str)
      in
        res == (Right $ escapedCharToChar str)

  prop "escapedString parses all the things except \"" $ do
    forAll genEscapedString $ \str ->
      let
        res = parseOnly (escapedString <* endOfInput) $ (BSC.pack str)
      in
        res == (Right $ replaceSlash str)

  prop "string parses all valid strings, surronded by \" with none in the middle" $ do
    forAll genString $ \x ->
      let
        res = parseOnly (Lib.string <* endOfInput) $ BSC.pack $ x
      in
        res == (Right $ trimQuotes $ replaceSlash x)

  prop "stringConcat parses all valid strings or ident combos" $ do
    forAll genStringConcat $ \x ->
      let
        res = parseOnly (stringConcat <* endOfInput) $ BSC.pack $ stringConcatToString $ x
      in
        res == (Right $ genStrConcatToParsed x)
