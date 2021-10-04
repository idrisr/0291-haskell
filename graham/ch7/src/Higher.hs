module Higher where

import Data.Char

mp :: (a->b) -> [a] -> [b]
mp f xs = [f x|x<-xs]

flter :: (a->Bool)->[a]->[a]
flter f xs = [x | x<-xs, f x]

flter1 :: (a->Bool)->[a]->[a]
flter1 _ [] = []
flter1 f (x:xs)
  | f x = x:(flter1 f xs)
  | otherwise = (flter1 f xs)

fldr :: (a->b->b)->b->[a]->b
fldr _ v [] = v
fldr f v (x:xs) = f x (fldr f v xs)
-- [1, 2, 3, 4]
-- (1:(2:(3:(4:p[]))))

snoc :: a -> [a] -> [a]
snoc x y = y ++ [x]

revers = fldr snoc []

fldl :: (a->b->b)->b->[a]->b
fldl _ v [] = v
fldl f v (x:xs) = fldl f (f x v) xs
-- [1, 2, 3, 4]
-- [((((1:[]):2):3):4)]

-- composition
-- B combinator
-- B = S(KS)S
cmp :: (b->c) -> (a->b) -> (a->c)
cmp f g = \x -> f (g x)

type Bit = Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2: int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
