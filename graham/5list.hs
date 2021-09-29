import Data.Char

-- lists
a = [x^2 | x <- [1..5]]
b = [(x, y) | x <-[1, 2, 3], y <-[4,5]]
c = [(x, y) | x <-[1..3], y <-[x..3]]

concat :: [[a]] -> [a]
concat xss = [x | xs <-xss, x<-xs]

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x, _) <-xs]

-- guards
factors :: Int -> [Int]
factors f = [x | x <- [1..f], mod f x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <-[1..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k',v)<-t, k==k']

-- zip
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x<=y | (x, y)<- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (i, x') <- zip [0..] xs, x'==x]

-- string comprehensions
lowers :: String -> Int
lowers xs = sum [1 | x<-xs, x>='a' && x <='z']

count :: Char -> String -> Int
count c xs = sum [1 | x<-xs, x==c]

let2int:: Char -> Int
let2int c = ord c - ord 'a'

int2let:: Int -> Char
int2let i = chr (i + ord 'a')

shift :: Int -> Char -> Char
shift n c 
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise  = c

table :: [Float]
table = 
    [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 
    0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 
    9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

encode :: Int -> String -> String
encode n xs = [shift n x | x<- xs] 

-- ratio of two numbers
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- vector of freqs, sums to 100
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack  :: String -> String
crack xs = encode (-factor) xs
    where 
        -- find index of min chi square
        factor = head (positions (minimum chis) chis)
        -- calc all chi squares
        chis = [chisqr (rotate n table') table | n<-[0..25]]
        -- create observed vector 
        table' = freqs xs

-- exercises
sumsq :: Int -> Int
sumsq n = sum [x^2 | x<-[1..n]]

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <-[0..x], y' <- [0..y]]

square :: Int -> [(Int, Int)]
square x = [(x', y') | x' <-[0..x], y' <- [0..x], x'/=y']

rep2 :: Int -> a -> [a]
rep2 i x = [x | _ <- [1..i]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x<-[1..n], 
                       y<-[1..n],
                       z<-[1..(x+y)], x^2+y^2==z^2]

perfects :: Int -> [Int]
perfects n = [i | i<-[1..n], (sum $ init $ factors i) == i]
