fac :: Int -> Int
fac 0 = 1
fac n | n>0 = n * fac (n-1)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n>0 = n + sumdown(n-1)

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n-1))
-- 2^3 = 2 * (2^2)
-- 2^2 = 2 * (2^1)
-- 2^1 = 2 * (2^0)
-- 2^0 = 1

euclid :: Int -> Int -> Int
euclid x y 
  | x==y = x
  | x<y = euclid (y-x) x
  | y<x = euclid (x-y) y

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs
-- len [1,2,3] = 1 + len[2, 3]
-- len [2, 3]  = 1 + len[3]
-- len [3]     = 1 + len[]
-- len []      = 1

drep :: Int -> [a] -> [a]
drep 0 xs = xs
drep _ [] = []
drep n (_:xs) = drep (n-1) xs

-- drep 3 [1, 2, 3, 4, 5] = drep 2 [2, 3, 4, 5]
-- drep 2 [2, 3, 4, 5   ] = drep 1 [3, 4, 5]
-- drep 1 [3, 4, 5      ] = drep 0 [4, 5]
-- drep 0 [4, 5]          = [4, 5]

inti :: [a] -> [a]
inti [_] = []
inti (x:xs) = x: inti xs

-- init [1, 2, 3] = 1: (inti [2, 3])
-- init [   2, 3] = 2: (inti [3])
-- init [      3] = []
-- init [       ] = []

an :: [Bool] -> Bool
an [] = True
an (x:xs) = x && an xs

conct :: [[a]] -> [a]
conct [] = []
conct (x:xs) = x ++ (conct xs)

repl :: Int -> a -> [a]
repl 0 _ = []
repl n x = x : (repl (n-1) x)

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (_:xs) n = nth xs (n-1)

elm :: Eq a => a -> [a] -> Bool
elm _ [] = False
elm f (x:xs) 
  | f == x = True
  | f /= x = elm f xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y  = x: (merge xs (y:ys))
  | y <= x = y: (merge (x:xs) ys)

halve :: [a] -> ([a], [a])
halve xs = (take h xs, drop h xs)
    where h = div (length xs) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ fst $ halve xs) (msort $ snd $ halve xs)

sm :: Num a => [a] -> a
sm [] = 0
sm (x:xs) = x + sm xs

tke :: Int -> [a] -> [a]
tke _ [] = []
tke 0 _  = []
tke n (x:xs) = x:(tke (n-1) xs)

lst :: [a] -> a
lst [x] = x
lst (x:xs) = lst xs
