{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text 
helloPerson n = "Hello " <> n <> "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?" 
    name <- TIO.getLine
    let statement = helloPerson name
    TIO.putStrLn statement
