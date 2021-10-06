module Dada where

type String = [Char]
type Pos = (Int, Int)
type Pair a = (a, a)
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k kv = head [v | (k', v) <-kv, k'==k]

-- datatype
data Move = East | West | North | South

-- new position from old and move
move :: Move -> Pos -> Pos
move East (x, y) = (x-1, y)
move West (x, y) = (x+1, y)
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)

-- new position from list of moves from old 
moves :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms $ move m p

-- reverse move
rev:: Move -> Move
rev East = West
rev West = East
rev North = South
rev South = North

-- data Shape
data Shape = Circle Float | Rect Float Float

-- func square
square :: Float -> Shape
square s = Rect s s 

-- func area
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect w h) = w * h

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (div x y)

safehead :: [a] -> Maybe a 
safehead [] = Nothing
safehead (x:xs) = Just x

data Nat = 
    Zero | 
    Succ Nat deriving (Show, Eq)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat $ n-1)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- add Zero (Succ Zero) = Zero
-- add SSSSZ SSZ = S (add SSSZ SSZ)
-- add SSSZ  SSZ = S (add SSZ SSZ)
-- add SSZ   SSZ = S (add SZ SSZ)
-- add SZ    SSZ = S (add Z SSZ)
-- add Z     SSZ = SSZ
