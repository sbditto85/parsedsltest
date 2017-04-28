module DslSpec where

import           Data.Attoparsec.ByteString  hiding (string)
import qualified Data.ByteString.Char8       as BSC
import qualified Data.Either                 as E
import qualified Data.List                   as L
import           Data.Semigroup              ((<>))
-- import qualified Debug.Trace                 as D
import           Generators
import           Lib
import           Test.Hspec
import           Test.QuickCheck
import           Test.Hspec.QuickCheck
-- import           Text.RawString.QQ

  
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
        res = parseOnly (digits <* endOfInput) $ BSC.pack $ show x
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

  prop "declaration parses all declarations" $ do
    forAll genDeclaration $ \d ->
      let
        res = parseOnly (declaration <* endOfInput) $ BSC.pack $ declarationToString $ d
      in
        res == (Right $ genDeclarationToParsed d)

  prop "httpGet works for all the url types" $ do
    forAll genHttpGet $ \h ->
      let
        res = parseOnly (httpGet <* endOfInput) $ BSC.pack $ httpGetToString $ h
      in
        res == (Right $ genHttpGetToParsed h)

  prop "method parses all type of methods" $ do
    forAll genMethod $ \m ->
      let
        res = parseOnly (method <* endOfInput) $ BSC.pack $ methodToString $ m
      in
        res == (Right $ genMethodToParsed m)

  prop "errorHandler parses all type of error handlers" $ do
    forAll genErrorHandler $ \eH ->
      let
        res = parseOnly (errorHandler <* endOfInput) $ BSC.pack $ errorHandlerToString $ eH
      in
        res == (Right $ genErrorHandlerToParsed eH)

  prop "handleError parses the handling of an error" $ do
    forAll genHandleError $ \hEs ->
      let
        res = parseOnly (handleError <* endOfInput) $ BSC.pack $ handleErrorToString hEs
      in
        res == (Right $ [ genHandleErrorToParsed hEs ])

  it "handleError will parse an empty string to an empty list" $
    let
      res = parseOnly (handleError <* endOfInput) $ ""
    in
      res == (Right [])

  prop "handleError parses the handling of multiple handleErrors" $ do
    forAll genHandleErrors $ \hEs ->
      let
        res = parseOnly (handleError <* endOfInput) $ BSC.pack $ mconcat $ L.intersperse " " $ handleErrorToString <$> hEs
      in
        res == (Right $ genHandleErrorToParsed <$> hEs)

  prop "action parses all actions" $ do
    forAll genAction $ \a ->
      let
        res = parseOnly (action <* endOfInput) $ BSC.pack $ actionToString a
      in
        res == (Right $ genActionToParsed a)

  prop "actions parses a group of declarations and actions" $ do
    forAll genActions $ \a ->
      let
        res = parseOnly (actions <* endOfInput) $ BSC.pack $ actionsToString a
      in
        res == (Right $ genActionsToParsed a)

  prop "withCameraId parses all the withCameraId possibilities" $ do
    forAll genWithCameraId $ \wCI ->
      let
        res = parseOnly (withCameraId <* endOfInput) $ BSC.pack $ withCameraIdToString wCI
      in
        res == (Right wCI)

  prop "withLiveUnitId parses all the withLiveUnitId possibilities" $ do
    forAll genWithLiveUnitId $ \wLUI ->
      let
        res = parseOnly (withLiveUnitId <* endOfInput) $ BSC.pack $ withLiveUnitIdToString wLUI
      in
        res == (Right wLUI)

  prop "initFuncId parses all the initFunc possibilities" $ do
    forAll genInitFunc $ \iF ->
      let
        res = parseOnly (initFunc <* endOfInput) $ BSC.pack $ initFuncToString iF
      in
        res == (Right iF)
