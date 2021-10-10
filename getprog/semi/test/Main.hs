import Test.QuickCheck
import Semi

prop_a :: Int -> Bool
prop_a x  = (a x) > x

data Point = Pt Int Int 
instance Show (Point) where
    show (Pt x y) = "{" ++ show x ++ "," ++ show y ++ "}"

instance Arbitrary (Color) where
  arbitrary = oneof [return Red, 
                      return Yellow, 
                      return Blue, 
                      return Green,
                      return Brown,
                      return Purple]

prop_sg_color :: Color->Color->Color->Bool
prop_sg_color x y z = (x <> y ) <> z == x <> (y <>z)

prop_sg_int :: Int->Int->Int->Bool
prop_sg_int x y z = (x <> y ) <> z == x <> (y <>z)

main :: IO ()
main = do
    quickCheck prop_a
    quickCheck prop_sg_color
    quickCheck prop_sg_int
