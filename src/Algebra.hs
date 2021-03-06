{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Algebra
  ( Algebra(..)
  , SimilarAlgebra(..)
  -- from makeFree
  , withCamera
  , stringConcat
  , httpGetRequest
  , responseJson
  -- end from makeFree
  , fakeProgram
  , printProgram
  , astToAlgebra
  , Result
  , interpretAlgebra
  , strProgram
  ) where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Aeson                       hiding (Result)
import qualified Data.ByteString.Lazy.Char8       as BSLC
import qualified Data.HashMap.Lazy                as MAP
import qualified Data.Text                        as T
import           Lib                              hiding (stringConcat)
import           Network.HTTP.Simple              hiding (Response)
import           Network.HTTP.Types.Status
import           Text.RawString.QQ

type ResultIdent = Ident
type CameraId = Int
type LiveUnitId = Int
type IdentStorage = MAP.HashMap String String

data Result
  = Result ResponseCode Value
  deriving (Eq, Show)

-- | Need some way to force a start and end thats what IdentStorage is being used to do
data Algebra next
  = WithCamera Ident (IdentStorage -> next)
  | StringConcat ResultIdent StringConcat IdentStorage (IdentStorage -> next)
  | HttpGetRequest (Maybe ResultIdent) StringConcat [ HandleError ] IdentStorage (IdentStorage -> next)
  | ResponseJson ResponseCode JsonObject [ HandleError ] IdentStorage (Result -> next)
  -- ^ figure out a way not to need this without use pure?? ... using it to glue the astToAlgebra middle and end if there is no middle
  deriving (Functor)

makeFree ''Algebra

newtype SimilarAlgebra =
  SimilarAlgebra (Free Algebra Result)

emptyStorage :: IdentStorage
emptyStorage = MAP.empty

instance Eq SimilarAlgebra where
  -- TODO: do this
  (SimilarAlgebra (Free (WithCamera camId1 next1))) == (SimilarAlgebra (Free (WithCamera camId2 next2))) =
    camId1 == camId2 && SimilarAlgebra (next1 emptyStorage) == SimilarAlgebra (next2 emptyStorage)
  
  (SimilarAlgebra (Free (WithCamera _ _))) == _ = False

  (SimilarAlgebra (Free (StringConcat resIdent1 strConcat1 identStorage1 next1))) ==
    (SimilarAlgebra (Free (StringConcat resIdent2 strConcat2 identStorage2 next2))) =
    resIdent1 == resIdent2
    && strConcat1 == strConcat2
    && SimilarAlgebra (next1 identStorage1) == SimilarAlgebra (next2 identStorage2)

  (SimilarAlgebra (Free (StringConcat _ _ _ _))) == _ = False

  (SimilarAlgebra (Free (HttpGetRequest mResIdent1 strConcat1 handleErrors1 identStorage1 next1))) ==
    (SimilarAlgebra (Free (HttpGetRequest mResIdent2 strConcat2 handleErrors2 identStorage2 next2))) = 
    mResIdent1 == mResIdent2
    && strConcat1 == strConcat2
    && handleErrors1 == handleErrors2
    && SimilarAlgebra (next1 identStorage1) == SimilarAlgebra (next2 identStorage2)

  (SimilarAlgebra (Free (HttpGetRequest _ _ _ _ _))) == _ = False

  (SimilarAlgebra (Free (ResponseJson resCode1 jsonObj1 handleErrors1 identStorage1 next1))) ==
    (SimilarAlgebra (Free (ResponseJson resCode2 jsonObj2 handleErrors2 identStorage2 next2))) =
    resCode1 == resCode2
    && jsonObj1 == jsonObj2
    && handleErrors1 == handleErrors2
    && identStorage1 == identStorage2
    -- && SimilarAlgebra (next1 1) == SimilarAlgebra (next2 2) -- TODO: figure out how to make this work
  (SimilarAlgebra (Free (ResponseJson _ _ _ _ _))) == _ = False

  (SimilarAlgebra (Pure a)) == (SimilarAlgebra (Pure b)) = a == b
  (SimilarAlgebra (Pure _)) == _ = False

instance Show SimilarAlgebra where
  show (SimilarAlgebra fA) = stringifyProgram fA

-- TODO: make AST/DSL to Algebra converter
astToAlgebra :: SystemCall -> Free Algebra Result
astToAlgebra (SystemCall initFunc' (Actions []) response' errorHandlers') =
  astToAlgebraStart initFunc' >>=
  \identStorage -> astToAlgebraEnd response' errorHandlers' identStorage
astToAlgebra (SystemCall initFunc' (Actions aOrDs) response' errorHandlers') = 
  astToAlgebraStart initFunc' >>=
  \identStorage -> astToAlgebraMiddle aOrDs identStorage >>=
  \identStorage' -> astToAlgebraEnd response' errorHandlers' identStorage'

astToAlgebraStart :: InitFunc -> Free Algebra IdentStorage
astToAlgebraStart initFunc' = do
  convertInitFunc initFunc'

astToAlgebraMiddle :: [ ActionOrDeclaration ] -> IdentStorage -> Free Algebra IdentStorage
astToAlgebraMiddle (aOrD:[]) identStorage = convertActions identStorage aOrD
astToAlgebraMiddle (aOrD:rest) identStorage = convertActions identStorage aOrD >>= astToAlgebraMiddle rest
astToAlgebraMiddle [] _ = error "Something terrible happened"

astToAlgebraEnd :: Response -> [ HandleError ] -> IdentStorage -> Free Algebra Result
astToAlgebraEnd response' errorHandlers' identStorage = convertResponseFunc response' errorHandlers' identStorage
    

convertInitFunc :: InitFunc -> Free Algebra IdentStorage
convertInitFunc (InitWithCameraId   (WithCameraId ident'))   = do
  withCamera ident'
convertInitFunc (InitWithLiveUnitId (WithLiveUnitId ident')) = error "do this"

convertActions :: IdentStorage -> ActionOrDeclaration -> Free Algebra IdentStorage
convertActions identStorage (IsAction action') = convertAction identStorage action'
convertActions identStorage (IsDeclaration decl) = convertDeclaration identStorage decl

convertAction :: IdentStorage -> Action -> Free Algebra IdentStorage
convertAction identStorage (Action assignment' (MethodHttpGet (HttpGet strConcat)) handleError') =
  let
    convertAssignment (Assignment ident') = Just ident'
    convertAssignment NoAssignment = Nothing
  in
    httpGetRequest (convertAssignment assignment') strConcat handleError' identStorage

convertDeclaration :: IdentStorage -> Declaration -> Free Algebra IdentStorage
convertDeclaration identStorage (Declaration ident' strConcat') =
  stringConcat ident' strConcat' identStorage

convertResponseFunc :: Response -> [ HandleError ] -> IdentStorage -> Free Algebra Result
convertResponseFunc (Response code json_) errorHandlers identStorage =
  responseJson code json_ errorHandlers identStorage

-- | concat a stringconcat
concatStringConcat :: IdentStorage -> StringConcat -> String
concatStringConcat identStorage (StrIdentConcat (Ident fromIdent) stringConcat') =
  let
    fromIdentValue = MAP.lookupDefault "" fromIdent identStorage
    rest = concatStringConcat identStorage stringConcat'
  in
    fromIdentValue ++ rest
concatStringConcat identStorage (StrConcat firstPart stringConcat') =
  firstPart ++ (concatStringConcat identStorage stringConcat')
concatStringConcat identStorage (StrIdent (Ident fromIdent)) =
  MAP.lookupDefault "" fromIdent identStorage
concatStringConcat _ (StrString str) =
  str

-- TODO: make interperter
interpretAlgebra :: CameraId -> LiveUnitId -> Free Algebra Result -> IO Result
interpretAlgebra camId liveUnitId algebra = 
  interpretAlgebraHelper algebra
  where
    interpretAlgebraHelper (Free (WithCamera (Ident cameraIdent) next)) = do
      let identStorage = MAP.singleton cameraIdent (show camId)
      interpretAlgebraHelper $ next identStorage
    interpretAlgebraHelper (Free (StringConcat (Ident resIdent) strConcat identStorage next)) = do
      let res = concatStringConcat identStorage strConcat
      interpretAlgebraHelper $ next $ MAP.insert resIdent res identStorage
    interpretAlgebraHelper (Free (HttpGetRequest mIdent strConcat handleErrors identStorage next)) = do
      let url = concatStringConcat identStorage strConcat
      req <- parseRequest url
      resp <- httpLBS req
      let success = statusIsSuccessful $ getResponseStatus resp
          identStorage' = maybe
                          identStorage
                          (\(Ident i) -> MAP.insert i (BSLC.unpack $ getResponseBody resp) identStorage)
                          mIdent
      case success of
        True ->
          interpretAlgebraHelper $ next identStorage'
        False ->
          error "do this"
    interpretAlgebraHelper (Free (ResponseJson responseCode (JsonObject (JsonParamList params)) handleErrors identStorage next)) = do
      let toValue JsonParam{..} = (T.pack jsonParamKey) .= (String $ T.pack $ concatStringConcat identStorage $ unJsonValue jsonParamValue)
          retVal = object $ toValue <$> params
      pure $ Result responseCode retVal
      --TODO: handle errors???
    interpretAlgebraHelper (Pure _) = error "Improper termination"



-- | fake program and program printer
fakeProgram :: Free Algebra Result
fakeProgram = do
  identStorage <- withCamera (Ident "camId")
  identStorage' <- httpGetRequest (Just (Ident "res")) (StrString "http://www.google.com") [] identStorage
  responseJson 200 (JsonObject (JsonParamList [])) [] identStorage'

strProgram :: String
strProgram = [r|withCameraId camId {
               url = "http://www.google.com";
               resp <- httpGet url : handleError 100 log "request to blah sucked";
             } responseJson 0 { "url" : resp } : handleError 103 log "the whole thing just sucked"|]


printProgram :: Free Algebra Result -> IO Result
printProgram (Free (WithCamera (Ident camIdent) next)) = do
  putStrLn $ "WithCamera " ++ camIdent
  printProgram $ next MAP.empty
printProgram (Free (StringConcat (Ident res) strConcat identStorage next)) = do
  putStrLn $ "StringConcat " ++ res ++ " " ++ (concatStringConcat identStorage strConcat)
  printProgram $ next identStorage
printProgram (Free (HttpGetRequest mIdent strConcat _ identStorage next)) = do
  putStrLn $ "HttpGetRequest " ++ (maybe "" (\(Ident str) -> str) mIdent) ++ " " ++ (concatStringConcat identStorage strConcat)
  printProgram $ next identStorage
printProgram (Free (ResponseJson code jsonObject' _ identStorage _)) = do
  putStrLn $ "ResponseJson " ++ (show code)
  pure $ Result code (Null)
printProgram (Pure _) = error "Improper termincation"



stringifyProgram :: Free Algebra Result -> String
stringifyProgram (Free (WithCamera _ next)) = do
  "WithCamera\n" ++ (stringifyProgram $ next MAP.empty)
stringifyProgram (Free (StringConcat _ _ identStorage next)) = do
  "StringConcat\n" ++ (stringifyProgram $ next identStorage)
stringifyProgram (Free (HttpGetRequest _ _ _ identStorage next)) = do
  "HttpGetRequest\n" ++ (stringifyProgram $ next identStorage)
stringifyProgram (Free (ResponseJson _ _ _ _ _)) = "ResponseJson\n"
stringifyProgram (Pure _) = error "Improper termincation"
