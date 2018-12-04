import qualified Data.Map                      as M
import           Data.List
import           Data.Maybe

main = do
    content <- readFile "./input.txt"
    let points = concat [ getPoints . parseLineShort $ l | l <- lines content ]
    let pointCount = incrementCountsFromPoints M.empty points
    let overlappingPoints = M.filter (>1) pointCount
    print $ M.size overlappingPoints

incrementCountsFromPoints :: M.Map (Int, Int) Int -> [(Int, Int)] -> M.Map (Int, Int) Int
incrementCountsFromPoints m [] = m
incrementCountsFromPoints m (p : ps) =
    incrementCountsFromPoints (M.insertWith (+) p 1 m) ps

getPoints :: (Int, Int, Int, Int) -> [(Int, Int)]
getPoints (x, y, w, h) = [ (x, y) | x <- [x .. (x + w)], y <- [y .. (y + h)] ]

slice from to xs = read (take (to - from + 1) (drop from xs)) :: Int

parseLine :: String -> (Int, Int, Int, Int, Int)
parseLine l =
    let idStart = 1
        idEnd   = (fromMaybe 0 $ '@' `elemIndex` l) - 2
        xStart  = (fromMaybe 0 $ '@' `elemIndex` l) + 2
        xEnd    = (fromMaybe 0 $ ',' `elemIndex` l) - 1
        yStart  = (fromMaybe 0 $ ',' `elemIndex` l) + 1
        yEnd    = (fromMaybe 0 $ ':' `elemIndex` l) - 1
        wStart  = (fromMaybe 0 $ ':' `elemIndex` l) + 2
        wEnd    = (fromMaybe 0 $ 'x' `elemIndex` l) - 1
        hStart  = (fromMaybe 0 $ 'x' `elemIndex` l) + 1
        hEnd    = (length l) - 1
    in  ( slice idStart idEnd l
        , slice xStart  xEnd  l
        , slice yStart  yEnd  l
        , slice wStart  wEnd  l
        , slice hStart  hEnd  l
        )


parseLineShort :: String -> (Int, Int, Int, Int)
parseLineShort l =
    let xStart  = (fromMaybe 0 $ '@' `elemIndex` l) + 2
        xEnd    = (fromMaybe 0 $ ',' `elemIndex` l) - 1
        yStart  = (fromMaybe 0 $ ',' `elemIndex` l) + 1
        yEnd    = (fromMaybe 0 $ ':' `elemIndex` l) - 1
        wStart  = (fromMaybe 0 $ ':' `elemIndex` l) + 2
        wEnd    = (fromMaybe 0 $ 'x' `elemIndex` l) - 1
        hStart  = (fromMaybe 0 $ 'x' `elemIndex` l) + 1
        hEnd    = (length l) - 1
    in  ( slice xStart xEnd l
        , slice yStart yEnd l
        , slice wStart wEnd l
        , slice hStart hEnd l
        )

