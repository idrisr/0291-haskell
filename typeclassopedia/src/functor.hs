-- Implement Functor instances for Either e and ((->) e).
-- {-# LANGUAGE NoImplicitPrelude #-}

import Test.QuickCheck

class Functorz f where 
    fmapz :: (a->b)->f a->f b

instance Functorz (Either a) where
    fmapz _ (Left x)  = Left x
    fmapz f (Right y) = Right (f y)

instance Functorz ((,,) a b) where
    fmapz f (x, y, z) = (x, y, f z)

instance Functorz ((->) a) where
    fmapz g h = g.h

instance Functorz ((,) e) where
    fmapz f (x, y) = (x, f y)

data Pair a = Pair a a

instance Functor (Pair) where
    fmap f (Pair x y) = Pair (f x) (f y)

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Functor ITree where
    fmap f (Leaf g) = Leaf (f.g)
    fmap f (Node xs) = Node(fmap (fmap f) xs)

prop_flaw1 :: Either Int Int -> Bool
prop_flaw1 x = fmap (id) x == x
prop_flaw2 :: (Int -> Int) -> (Int -> Int) -> Either Int Int -> Bool
prop_flaw2 g h x = fmapz (g.h) x == (fmap g . fmap h)  x

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 500 } prop_flaw1
    quickCheckWith stdArgs { maxSuccess = 500 } $ prop_flaw2 (+1) (*3)

data K a = K (a -> Int)

ffmap :: (Functor f, Functor j) => (a -> b) -> j (f a) -> j (f b)
ffmap = fmap.fmap

instance Functorz [] where
    fmapz _ _ = []

-- functor laws
-- fmapz id = id
-- fmapz (g.h) = fmapz g . fmapz h
