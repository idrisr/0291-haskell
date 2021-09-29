not :: Bool -> Bool
not False = True
not True = False

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

yo :: Fractional a => a -> a -> a
yo x y = x + y / x

halve :: [a] -> ([a], [a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

third0 :: [a] -> a
third0 (_:_:z:xs) = z

third1 :: [a] -> a
third1 xs = head (tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

safetail0 :: [a] -> [a]
safetail0 xs = if null xs then [] else tail xs

safetail1 :: [a] -> [a]
safetail1 xs 
  | null xs  = []
  | otherwise  = tail xs

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 xs = tail xs

(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True |||| _ = True

ld :: Int -> Int
ld x 
  | x*2>9 = x*2-9
  | otherwise = x*2

-- luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (ld b + ld c + ld d) 10 == 0
