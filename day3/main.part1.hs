import qualified Data.Map                      as M
import           Data.List
import           Data.Maybe

main = do
    content <- readFile "./input.txt"
    let pointsPerLine = [ getPoints . parseLineShort $ l | l <- lines content ]
    --print pointsPerLine
    let points            = concat pointsPerLine
    let pointCount        = incrementCountsFromPoints M.empty points
    --print pointCount
    let overlappingPoints = M.filter (> 1) pointCount
    --print overlappingPoints
    print $ M.size overlappingPoints

incrementCountsFromPoints
    :: M.Map (Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int) Int
incrementCountsFromPoints m [] = m
incrementCountsFromPoints m (p : ps) =
    incrementCountsFromPoints (M.insertWith (+) p 1 m) ps

getPoints :: (Int, Int, Int, Int) -> [(Int, Int)]
getPoints (x, y, w, h) =
    [ (x, y) | x <- [x .. (x + w - 1)], y <- [y .. (y + h - 1)] ]

slice from to xs = read (take (to - from + 1) (drop from xs)) :: Int

parseLine :: String -> (Int, Int, Int, Int, Int)
parseLine l =
    let idStart = 1
        idEnd   = getIndex '@' - 2
        xStart  = getIndex '@' + 2
        xEnd    = getIndex ',' - 1
        yStart  = getIndex ',' + 1
        yEnd    = getIndex ':' - 1
        wStart  = getIndex ':' + 2
        wEnd    = getIndex 'x' - 1
        hStart  = getIndex 'x' + 1
        hEnd    = (length l) - 1
    in  ( slice idStart idEnd l
        , slice xStart  xEnd  l
        , slice yStart  yEnd  l
        , slice wStart  wEnd  l
        , slice hStart  hEnd  l
        )
    where getIndex x = fromMaybe 0 $ x `elemIndex` l


parseLineShort :: String -> (Int, Int, Int, Int)
parseLineShort l =
    let xStart = getIndex '@' + 2
        xEnd   = getIndex ',' - 1
        yStart = getIndex ',' + 1
        yEnd   = getIndex ':' - 1
        wStart = getIndex ':' + 2
        wEnd   = getIndex 'x' - 1
        hStart = getIndex 'x' + 1
        hEnd   = (length l) - 1
    in  ( slice xStart xEnd l
        , slice yStart yEnd l
        , slice wStart wEnd l
        , slice hStart hEnd l
        )
    where getIndex x = fromMaybe 0 $ x `elemIndex` l

