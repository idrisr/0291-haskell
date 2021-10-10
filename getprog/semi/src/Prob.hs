module Prob where

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable es ps = PTable es nps
    where tp = sum ps
          nps = map (\x -> x/tp) ps

showPair:: String -> Double -> String
showPair e p = mconcat [e, "|", show p, "\n"]

instance Show PTable where
    show (PTable es ps) = mconcat pairs
        where pairs = zipWith showPair es ps

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = zipWith f newL1 cycledL2
    where nToAdd = length l2
          repleatedL1 = map (take nToAdd . repeat ) l1
          newL1 = mconcat repleatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) p (PTable [] []) = p
    (<>) (PTable [] []) p = p
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs  = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

dice :: PTable
dice = createPTable ["1", "2", "3", "4", "5", "6"] $ replicate 6 (1 / 6)
