module Main where
import Test.QuickCheck

data D1 = D1 Int deriving Show
data D2 = D2 !Int deriving Show

type S = Int
newtype N = N Int

d1 :: Num p => D1 -> p
d1 (D1 i) = 42

d2 :: Num p => D2 -> p
d2 (D2 i) = 42

s :: Num a => a -> a
s i = 42
n (N i) = 42

main :: IO (Char)
main = do
    a1 <- (generate $ elements "idris")
    print(11)
    return a1

act :: IO (Char, Char, Char)
act = do x <- getChar
         y <- getChar
         return (x, y, 's')

myany :: (a -> Bool) -> [a] -> Bool
myany testFunc = (foldr (||) True) . (map testFunc)

myall :: (a -> Bool) -> [a] -> Bool
myall testFunc = (foldr (&&) True) . (map testFunc)
