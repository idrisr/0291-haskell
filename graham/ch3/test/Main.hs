module Main where

import Test.QuickCheck
import Stuff

prop_twice :: Int -> Bool
prop_twice x = (twice (*2) x) == x * 2 * 2

main :: IO ()
main = do
    quickCheck prop_twice
