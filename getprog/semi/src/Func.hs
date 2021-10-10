import qualified Data.Map as Map

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyvals
    where keys = [1, 2, 3]
          vals = [leftArm,rightArm,robotHead]
          keyvals = zip keys vals

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just a) = Just (reverse a)
reverseMaybe Nothing = Nothing

data RobotPart = RobotPart
    { name :: String
    , description :: String
    , cost :: Double
    , count :: Int
    } deriving Show

leftArm = RobotPart
    { name = "left arm"
    , description = "left arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    } 

rightArm = RobotPart
    { name = "right arm"
    , description = "right arm for @*!# hand gestures"
    , cost = 525.00
    , count = 15
    } 

robotHead = RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    } 

type HTML = String
renderHTML :: RobotPart -> HTML
renderHTML part = mconcat ["<h2>",partName, "</h2>"
                          ,"<p><h3>desc</h3>",partDesc
                          ,"</p><p><h3>cost</h3>"
                          ,partCost
                          ,"</p><p><h3>count</h3>"
                          ,partCount,"</p>"]
  where partName = name part
        partDesc = description part
        partCost = show (cost part)
        partCount = show (count part)

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHTML :: Maybe HTML
partHTML = renderHTML <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHTML :: [HTML]
allPartsHTML = renderHTML <$> allParts

htmlPartsDB :: Map.Map Int HTML
htmlPartsDB  = renderHTML <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO HTML
htmlSnippet  = renderHTML <$> leftArmIO

data Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box a) = Box (f a)

myBox :: Box Int
myBox = Box 1

wrapped = fmap Box myBox
unwrap :: Box a -> a
unwrap (Box a) = a

