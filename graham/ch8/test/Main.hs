module Main where

import Test.QuickCheck
import Dada

yo :: Int -> Int
yo x = x + 1

propyo :: Int -> Bool
propyo x = (>) (yo x) x

propadd :: Nat -> Nat -> Bool
propadd x y = (add x y) == (add y x)

newtype Gnat = N Int

main :: IO ()
main = do
    quickCheck propyo
    quickCheck propadd
