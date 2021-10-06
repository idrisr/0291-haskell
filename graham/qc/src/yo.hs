import Test.QuickCheck

generate $ elements [(1::Int)..2]
-- data Eq a => Set a = NilSet
      -- | ConsSet a (Set a)

main :: IO ()
main = do 
    generate $ elements [(1::Int)..2]
