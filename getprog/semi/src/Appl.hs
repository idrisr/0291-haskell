import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham", (42.6054, -70.7829))
                          ,("Innsmouth", (42.8250, -70.8150))
                          ,("Carcosa", (29.9714, -90.7694))
                          ,("Chicago", (41.8818, -87.623177))
                          ,("New York", (40.7776, -73.9691))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> LatLong
latLongToRads (lat, long) = (rlat, rlong)
    where rlat = toRadians lat
          rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where (rlat1, rlong1) = latLongToRads coords1
          (rlat2, rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just d) = putStrLn (show d ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a+b)
addMaybe _ _ = Nothing

ny :: LatLong
ny = (40.7776, -73.9691)

distFromNY :: LatLong -> Double
distFromNY = haversine ny

maybeInc = (+) <$> Just 1
a = (++) <$> Just "cats"

main :: IO ()
main = do
    putStrLn "Enter the starting city name: "
    startingInput <- getLine
    let startingCity = Map.lookup startingInput locationDB
    putStrLn "Enter the destination city name: "
    destInput <- getLine
    let destCity = Map.lookup destInput locationDB
    let distance = haversine <$> startingCity <*> destCity
    printDistance distance

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x (min y z)

readInt :: IO String
readInt = read <$> getLine

-- minOfInts :: IO Int
-- minOfInts = minOfThree <$> readInt <*> readInt <*> readInt
