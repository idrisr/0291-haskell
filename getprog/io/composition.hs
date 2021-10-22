import Test.QuickCheck

a f e g = foldr f e . map g 

b f e g a b = foldr f' e
    where f' a b = f (g a) b

(>:) :: Int -> Int -> Int
(>:) x y = (x+y) * 2

(<:) :: Int -> Int -> Int
(<:) x y = (x+y) * 2

prop_assoc :: Int -> Int -> Int -> Bool
prop_assoc x y z = (x >: y) >: z == x >: (y >: z)

infixl 6 >:
infixr 0 <:

-- fmap takes a 
-- 1) a to b func
-- 2) some functor

-- so first we pass in const, which gets const 
-- from a c->d->c to a d->c.
-- then each value in the functor is mapped into the 
-- func, and because it's const, you get first value passed in

c = (fmap.const) 1 (Left 10)
d = (fmap.const) 1 (Right 10)
e = (fmap.const) [2..9] (Just [1..10])
