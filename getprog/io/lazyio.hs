{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Lazy 

toInts :: T.Text -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    userInput <- getContents
    let numbers = toInts userInput
    print (sum numbers)
