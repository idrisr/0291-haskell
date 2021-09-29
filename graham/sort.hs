qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]

qrev :: Ord a => [a] -> [a]
qrev [] = []
qrev (x:xs) = qrev larger ++ [x] ++ qrev smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

-- (++) :: [a] -> [a] -> [a]

prodct :: Num a => [a] -> a
prodct [] = 1
prodct (x:xs) = x * (prodct xs)

factorial n = product [1..n]

a = b + c + d
    where b = 11
          c = 22
          d = 33


n = a `div` length xs 
    where
        a = 10
        xs = [1,2,3,4,5]

lst xs = xs !! (length xs -1)
lst2 xs = reverse xs !! 0

ini xs = take (length xs - 1) xs

ini2 xs = reverse (drop 1 (reverse xs))
