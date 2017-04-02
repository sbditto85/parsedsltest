module Main where

import Data.Attoparsec.ByteString
import Lib

main :: IO ()
main = do
  let res = parseOnly ident "alskdjfoaweiflsdj234lasjdfoj(*&#$lasdkjfalsdj"
  someFunc
  putStrLn $ show res
