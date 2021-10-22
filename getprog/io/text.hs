{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

firstWord :: String
firstWord = "cryptography"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

b :: T.Text
b = "some\ntext for\t you"

bT :: T.Text
bT = "simple"

main :: IO ()
main = do
    print aWord
