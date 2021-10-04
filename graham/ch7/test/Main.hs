module Main where

import Higher
import Test.QuickCheck

prop_code :: String -> Bool
prop_code x = decode (encode x) == x

prop_i2b :: Int -> Bool
prop_i2b x = (bin2int $ int2bin x) == x

dum :: Int -> Int
dum a = a + 4 * 9

prop_mp :: [Int] -> Bool
prop_mp xs = (mp dum xs) == (map dum xs)

main :: IO ()
main = do
  quickCheck prop_code
  -- quickCheck prop_i2b
  quickCheck prop_mp
