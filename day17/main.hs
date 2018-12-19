import           Data.Char
import qualified Data.HashSet                  as S

type Location = (Int,Int)

main = do
    content <- readFile "test.txt"
    let clay = S.fromList $ readInput $ lines content
    let water = S.empty
    let water' = fillWater (500,0) clay water
    print $ S.size water'

fillWater :: (Int, Int) -> S.HashSet Location -> S.HashSet Location -> S.HashSet Location
fillWater current@(x, y) clay water
    | y > 500 = water
    | canMoveDown = fillWater downOne clay (S.insert downOne water)
    | canMoveLeft && canMoveRight = fillWater
        rightOne
        clay
        (S.insert rightOne (fillWater leftOne clay (S.insert leftOne water)))
    | canMoveLeft = fillWater leftOne clay (S.insert leftOne water)
    | canMoveRight = fillWater rightOne clay (S.insert rightOne water)
    | otherwise = water
  where
    downOne      = (x, y + 1)
    canMoveDown  = canMove downOne clay water
    leftOne      = (x - 1, y)
    canMoveLeft  = canMove leftOne clay water
    rightOne     = (x - 1, y)
    canMoveRight = canMove rightOne clay water


canMove :: (Int, Int) -> S.HashSet Location  -> S.HashSet Location -> Bool
canMove location clay water =
    not $ location `S.member` clay || location `S.member` water

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
