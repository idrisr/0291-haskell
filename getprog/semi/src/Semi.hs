module Semi where

instance Semigroup Int where
    (<>) x y = x + y

a :: Int -> Int
a x = x + 1

data Color = Red | Yellow | Blue | Green | Orange | Brown | Purple 
    deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Yellow = Orange
    (<>) Yellow Red = Orange
    (<>) Blue Red = Purple
    (<>) Red Blue = Purple
    (<>) Blue Yellow = Green
    (<>) Yellow Blue = Green
    (<>) a b 
      | a == b = a
      | all (flip elem [Red, Yellow, Orange]) [a, b] = Orange
      | all (flip elem [Red, Blue, Purple]) [a, b] = Purple
      | all (flip elem [Yellow, Blue, Green]) [a, b] = Green
      | otherwise = Brown
