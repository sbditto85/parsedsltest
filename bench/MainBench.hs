{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = defaultMain $ [ bench "fib" $ whnf fib 20
                     ]
