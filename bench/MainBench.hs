{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.Attoparsec.ByteString  hiding (string)
import           Criterion.Main
import qualified Data.ByteString.Char8       as BSC
import           Generators
import           Lib

d :: Declaration
d = Declaration (Ident "lulz") (StrIdentConcat (Ident "you") (StrString "are awesome!")) 

input :: BSC.ByteString
input = BSC.pack $ declarationToString d

main :: IO ()
main = defaultMain $ [ bench "declaration parse" $ whnf
                       (parseOnly (declaration <* endOfInput))
                       (input)
                     ]
