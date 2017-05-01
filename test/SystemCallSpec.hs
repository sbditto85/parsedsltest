module SystemCallSpec
  ( spec
  ) where

import           Data.Attoparsec.ByteString  hiding (string)
import qualified Data.ByteString.Char8       as BSC
import qualified Data.Either                 as E
import           Lib
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

  it "convert simple program to actual DSL" $ do
    let program = [r|withCameraId camId {
                    url = "http://test.example.com/my/endpoint" ++ camId;
                    resp <- httpGet url : handleError 100 log "request to blah sucked";
                    } responseJson 0 { "url" : url } : handleError 103 log "the whole thing just sucked"|]
        res = parseOnly (systemCall <* endOfInput) $ BSC.pack program
    print res

    pendingWith "need to do this"
