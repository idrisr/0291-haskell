import System.Environment
import Control.Monad

myReplicateM n m = mapM (\_ -> m) [1..n]

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    input <- getContents
    let numbers = toInts input
    print (sum numbers)
