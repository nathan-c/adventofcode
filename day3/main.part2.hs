import qualified Data.Map                      as M
import           Data.List
import           Data.Maybe

main = do
    content <- readFile "./input.txt"
    let pointsPerLine = [ getPoints . parseLine $ l | l <- lines content ]
    let points        = concat (map stripId pointsPerLine)
    let pointCount    = incrementCountsFromPoints M.empty points
    let (soloId, pts) = (filter (checkSolo pointCount) pointsPerLine) !! 0
    print soloId

checkSolo :: M.Map (Int, Int) Int -> (Int, [(Int, Int)]) -> Bool
checkSolo pointCount (_, pts) =
    all (\pt -> (M.lookup pt pointCount) == Just 1) pts

incrementCountsFromPoints
    :: M.Map (Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int) Int
incrementCountsFromPoints m [] = m
incrementCountsFromPoints m (p : ps) =
    incrementCountsFromPoints (M.insertWith (+) p 1 m) ps

stripId :: (Int, [(Int, Int)]) -> [(Int, Int)]
stripId (i, pts) = pts

getPoints :: (Int, Int, Int, Int, Int) -> (Int, [(Int, Int)])
getPoints (i, x, y, w, h) =
    (i, [ (x, y) | x <- [x .. (x + w - 1)], y <- [y .. (y + h - 1)] ])

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
