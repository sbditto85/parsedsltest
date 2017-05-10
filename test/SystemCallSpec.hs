module SystemCallSpec
  ( spec
  ) where

import           Algebra
import           Control.Monad.Free
import           Data.Attoparsec.ByteString  hiding (string)
import qualified Data.ByteString.Char8       as BSC
import qualified Data.Either                 as E
import           Lib                         hiding (stringConcat)
import           Test.Hspec
import           Text.RawString.QQ

spec :: Spec
spec = do
  it "parses out a simple basic program" $ do
    let program = [r|withCameraId camId {
                    url = "http://" ++ cameraPublicIp ++ ":" ++ cameraPublicPort ++ "/my/endpoing";
                    resp <- httpGet url : handleError 100 log "request to blah sucked";
                    } responseJson 0 { "url" : url } : handleError 103 log "the whole thing just sucked"|]
        res = parseOnly (systemCall <* endOfInput) $ BSC.pack program

    shouldBe True (E.isRight res)

  it "convert simple program to actual algebra" $ do
    let program = [r|withCameraId camId {
                    url = "http://test.example.com/my/endpoint" ++ camId;
                    resp <- httpGet url : handleError 100 log "request to blah sucked";
                    } responseJson 0 { "url" : url } : handleError 103 log "the whole thing just sucked"|]
        res = parseOnly (systemCall <* endOfInput) $ BSC.pack program
    let prog = either (error "didn't work") id res
        convertedProg = astToAlgebra prog

    -- printProgram convertedProg

    let viaFree :: Free Algebra ()
        viaFree = do
          iS <- withCamera (Ident "camId")
          iS' <- stringConcat (Ident "url") (StrConcat "http://test.example.com/my/endpoint" (StrIdent (Ident "camId"))) iS
          iS'' <- httpGetRequest (Just (Ident "resp")) (StrIdent (Ident "url")) [ HandleError 100 (ErrorHandlerLog (StrString "request to blah sucked")) ] iS'
          responseJson 0 (JsonObject (JsonParamList [ JsonParam { jsonParamKey = "url"
                                                                , jsonParamValue = JsonValue (StrIdent (Ident "url"))
                                                                }
                                                    ]
                                     )
                         ) [ HandleError 103 (ErrorHandlerLog (StrString "the whole thing just sucked")) ] iS''

    -- printProgram viaFree

    -- print $ (SimilarAlgebra convertedProg) == (SimilarAlgebra viaFree)

    shouldBe (SimilarAlgebra convertedProg) (SimilarAlgebra viaFree)
