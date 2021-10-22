module Main where

import Test.QuickCheck

join :: Monad m => m (m a) -> m a
join x = x >>= id

prop_law_1 :: [[[Int]]] -> Bool
prop_law_1 xs = a == b 
    where
        a = join $ fmap join xs
        b = join $ join xs

prop_law_1a :: Maybe (Maybe (Maybe Char)) -> Bool
prop_law_1a xs = a == b 
    where
        a = join $ fmap join xs
        b = join $ join xs
        
prop_law_2 :: [[Int]] -> Bool
prop_law_2 xs = and [(a == b), (b == c)]
    where
        a = join $ fmap return xs
        b = join $ return xs
        c = id xs

prop_law_2a :: Maybe (Maybe Int) -> Bool
prop_law_2a xs = and [(a == b), (b == c)]
    where
        a = join $ fmap return xs
        b = join $ return xs
        c = id xs

-- prop_law_3 :: Int -> Bool
-- prop_law_3 x = a == b
    -- where
        -- a = (return . (+123)) $ x
        -- b = fmap (+123) (return x)::Int

(!.!) :: (b->c) -> (a->b) -> a->c
(!.!) f g =  \x -> f $ g x

cunst :: a -> b -> a
cunst x y = x

fc :: Functor f => b -> f a -> f b
fc = fmap.const

main = do
    quickCheck prop_law_1
    quickCheck prop_law_1a
    quickCheck prop_law_2
    quickCheck prop_law_2a
    -- quickCheck prop_law_3
