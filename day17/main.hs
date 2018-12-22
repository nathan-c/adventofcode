import           Data.Char
import Data.List
import qualified Data.Set                  as S

type Location = (Int,Int)
type Bounds = (Int,Int,Int,Int)

main = do
    content <- readFile "input.txt"
    let clayList = readInput $ lines content
    let clay = S.fromList clayList
    let water = (S.empty,S.empty)
    let bounds@(_,_,miny,maxy) = getBounds clayList
    print bounds
    --showSquares bounds clay water
    let water'@(standingWater, runningWater) = fillWater bounds (500,0) clay water
    let allWater = S.union standingWater runningWater
    putStrLn ""
    showSquares bounds clay water'

    print $ S.size allWater

fillWater :: Bounds -> (Int, Int) -> S.Set Location -> (S.Set Location,S.Set Location) -> (S.Set Location,S.Set Location)
fillWater bounds@(minx,maxx,_, maxy) current@(x, y) clay water@(standingWater, runningWater)
    | y >= maxy = water
    | canMoveDown = 
        let downWater@(standingWater',_) = fillWater bounds downOne clay (standingWater,(S.insert downOne runningWater)) 
        in  if downOne `S.member` standingWater' then
                fillWater bounds current clay downWater 
            else
                downWater
    | canMoveLeft && canMoveRight =
                let (leftStanding,leftRunning) = fillWater bounds leftOne clay (standingWater,S.insert leftOne runningWater)
                    (rightStanding,rightRunning) = fillWater bounds rightOne clay (standingWater,S.insert rightOne runningWater)
                    standing' = S.insert current (S.union rightStanding leftStanding)
                    running' = (S.union rightRunning leftRunning)
                in  if isFullRow (minx,maxx) current standing' running' then
                        (standing', running')
                    else
                        (standingWater, running')          
    | canMoveLeft   = moveLeft bounds current leftOne clay water 
    | canMoveRight  = moveRight bounds current rightOne clay water
    | otherwise     = (S.insert current standingWater, runningWater)
  where
    downOne      = (x, y + 1)
    canMoveDown  = canMove downOne clay water
    leftOne      = (x - 1, y)
    canMoveLeft  = canMove leftOne clay water
    rightOne     = (x + 1, y)
    canMoveRight = canMove rightOne clay water

moveLeft bounds current leftOne clay (standingWater,runningWater) = 
    let
        water'@(standingWater', runningWater') = fillWater bounds leftOne clay (standingWater,S.insert leftOne runningWater)
    in  if standingWater /= standingWater' then 
            (S.insert current standingWater', runningWater')
        else
            water'

moveRight bounds current rightOne clay (standingWater,runningWater) = 
    let
        water'@(standingWater', runningWater') = fillWater bounds rightOne clay (standingWater,S.insert rightOne runningWater)
    in  if standingWater /= standingWater' then 
            (S.insert current standingWater', runningWater')
        else
            water'

isFullRow :: (Int,Int)-> (Int,Int) -> S.Set Location -> S.Set Location -> Bool
isFullRow (minx,maxx) p@(x,y) standingWater runningWater = 
    let 
        row = (S.fromList [(x',y)|x'<-[minx..maxx]])
        standing' = getNeighbours p $ row `S.intersection` standingWater
        running' =  getNeighbours p $ row `S.intersection` runningWater
    in standing' == running'
 
getNeighbours :: Location -> S.Set Location -> [Location]
getNeighbours p@(x,y) wholeRow
    | p `S.member` wholeRow = 
        let 
            left = getLeftNeighbours (x-1,y) wholeRow
            right = getRightNeighbours (x+1,y) wholeRow
        in  left ++ [p] ++ right
    | otherwise = []


getRightNeighbours :: Location -> S.Set Location -> [Location]
getRightNeighbours p@(x,y) wholeRow
    | p `S.member` wholeRow = p:getRightNeighbours (x+1,y) wholeRow
    | otherwise = []


getLeftNeighbours :: Location -> S.Set Location -> [Location]
getLeftNeighbours p@(x,y) wholeRow
    | p `S.member` wholeRow = p:getLeftNeighbours (x-1,y) wholeRow
    | otherwise = []


canMove :: (Int, Int) -> S.Set Location  -> (S.Set Location,S.Set Location) -> Bool
canMove location clay (standingWater, runningWater) =
    not $ location `S.member` clay || location `S.member` standingWater || location `S.member` runningWater

readInput :: [String] -> [(Int, Int)]
readInput []       = []
readInput (l : ls) = [ (x, y) | y <- [y1 .. y2], x <- [x1 .. x2] ]
    ++ readInput ls
  where
    [single, range] = words l
    (x1    , x2   ) = if head single == 'x'
        then let i = read (filter isDigit single) :: Int in (i, i)
        else splitRange range
    (y1, y2) = if head single == 'y'
        then let i = read (filter isDigit single) :: Int in (i, i)
        else splitRange range

--y=1077..1087
splitRange :: String -> (Int, Int)
splitRange range = (start, end)
  where
    range'             = drop 2 range
    (startStr, endStr) = span isDigit range'
    start              = read startStr :: Int
    end                = read (drop 2 endStr) :: Int

showSquares :: (Int,Int,Int,Int) -> S.Set Location -> (S.Set Location, S.Set Location) -> IO()
showSquares (minx,maxx,miny,maxy) clay (standingWater, runningWater)  = do
    putStrLn $ foldl1 (\agg a -> agg ++ "\n" ++ a) [[ showCell (x,y) clay standingWater runningWater  | x<-[minx-1..maxx+1]]| y<-[miny-1..maxy+1]]

showCell :: (Int,Int) -> S.Set Location -> S.Set Location -> S.Set Location -> Char
showCell p clay standingWater runningWater 
    | p `S.member` clay = '#'
    | p `S.member` standingWater = '~'
    | p `S.member` runningWater = '|'
    | otherwise = '.'


getBounds :: [(Int,Int)] -> (Int,Int,Int,Int)
getBounds clay = 
    let minx = minimum $ map fst clay
        maxx = maximum $ map fst clay
        miny = minimum $ map snd clay
        maxy = maximum $ map snd clay
    in (minx,maxx,miny,maxy)