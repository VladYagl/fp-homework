module Town
       ( Building
       , Castle
       , House
       , Lord
       , Town
       , Walls
       , emptyTown
       , house
       , town

       , addLord
       , buildCastle
       , buildHouse
       , buildInTown
       , buildWalls
       ) where

data BuildResult a = Success a | Failure String deriving Show

data Castle = Castle deriving Show
data Walls = Walls deriving Show
data Lord = Lord deriving Show
data Building = Church | Library | None deriving Show
data House = BuildHouse Int deriving Show

familySize :: House -> Int
familySize (BuildHouse size) = size

house :: Int -> BuildResult House
house civs
    | civs > 0 && civs < 5 = Success (BuildHouse civs)
    | otherwise = Failure ("Wrong family size: " ++ (show civs))

data Town = BuildTown
    { castle   :: Maybe Castle
    , building :: Building
    , lord     :: Maybe Lord
    , walls    :: Maybe Walls
    , houses   :: [House]
    } deriving Show


town :: Maybe Castle -> Building -> Maybe Lord -> Maybe Walls -> [House] -> BuildResult Town
town castle_ building_ lord_ walls_ houses_
    | length houses_ > 0 =
        case walls_ of
            Just _ -> case castle_ of
                Just _  -> Success (BuildTown castle_ building_ lord_ walls_ houses_)
                Nothing -> Failure "There are wall but no castle"
            Nothing -> Success (BuildTown castle_ building_ lord_ walls_ houses_)
    | otherwise = Failure "Not enough houses"

emptyTown :: Town
emptyTown = BuildTown Nothing None Nothing Nothing [BuildHouse 1]

buildCastle :: Town -> Castle -> BuildResult Town
buildCastle curTown newCastle = case castle curTown of
    Just _  -> Failure "There are castle already"
    Nothing -> Success (curTown { castle = Just newCastle })

buildInTown :: Town -> Building -> BuildResult Town
buildInTown _ None                                        = Failure "Can't build nothing"
buildInTown curTown@(BuildTown{ building = None }) newBuilding = Success (curTown{building = newBuilding})
buildInTown curTown _ = Failure ("There are already " ++ (show (building curTown)) ++ " in Town")

buildHouse :: Town -> Int -> BuildResult Town
buildHouse curTown@(BuildTown{houses = curHouses}) size = do
    let newHouse = house size
    case newHouse of
        Success checkedHouse -> Success (curTown{houses = curHouses ++ [checkedHouse]})
        Failure message      -> Failure message

addLord :: Town -> Lord -> BuildResult Town
addLord curTown@(BuildTown{castle = Just _, lord = Nothing}) newLord = Success(curTown{lord = Just newLord})
addLord BuildTown{castle = Nothing} _ = Failure "There is no castle in town"
addLord BuildTown{lord = Just _} _ = Failure "There is already lord in town"

buildWalls :: Town -> Walls -> BuildResult Town
buildWalls curTown@(BuildTown{houses = curHouses, walls = Nothing}) newWalls
    | sum (map familySize curHouses) >= 10 = Success (curTown{walls = Just newWalls})
    | otherwise = Failure "Not enough civilians"
buildWalls BuildTown{walls = Just _} _ = Failure "There are already walls in town"
