import           Data.List
import           Data.List.Unique
import           Data.Maybe
import           Data.Char
import qualified KdTreeInt                     as Kd
import qualified Data.Map                      as M

-- kd tree def
data Point2d = Point2d {p2x :: Int, p2y :: Int} deriving (Eq, Ord, Show)

instance Kd.Point Point2d where
    dimension _ = 2
    coord 0 p = p2x p
    coord 1 p = p2y p

-- / kd tree def



main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    content <- readFile "./input.txt"
    let coords                     = [ readLine x | x <- lines content ]
    let Bounds minx maxx miny maxy = getBounds emptyBounds coords
    let points                     = [ Point2d x y | (x, y) <- coords ]
    let tree                       = Kd.fromList points
    let area                       = M.fromList $ zip points $ repeat 0
    let allPoints = [ Point2d x y | y <- [miny .. maxy], x <- [minx .. maxx] ]
    let boundaryPoints =
            [ Point2d x y | y <- [miny, maxy], x <- [minx .. maxx] ]
                ++ [ Point2d x y | y <- [miny .. maxy], x <- [minx, maxx] ]
    let infinitePoints = sortUniq $ mapMaybe (getNearest tree) boundaryPoints
    let labels         = M.fromList $ zip points (['A' .. 'Z'] ++ ['a' .. 'z'])
    let areas          = incrementCounts tree area allPoints
    print $ sort $ map (labels M.!) infinitePoints
    let areasWithBoundariesRemoved =
            filter (\x -> notElem (fst x) infinitePoints) $ M.toList areas
    --print $ sortOn snd $ map (\(a,b) -> (labels M.! a, b)) areasWithBoundariesRemoved
    print $ maximumBy (\x y -> (snd x) `compare` (snd y))
                      areasWithBoundariesRemoved

part2 :: IO ()
part2 = do
    content <- readFile "./input.txt"
    let coords                     = [ readLine x | x <- lines content ]
    let Bounds minx maxx miny maxy = getBounds emptyBounds coords
    let points                     = [ Point2d x y | (x, y) <- coords ]
    let allCoords = [ Point2d x y | y <- [miny .. maxy], x <- [minx .. maxx] ]
    let labels         = M.fromList $ zip points (['A' .. 'Z'] ++ ['a' .. 'z'])
    let allChars = [getPointString labels p |p<- allCoords]
    --putStrLn $ splitArea (maxx - minx +1) allChars
    let allSums = map (\x -> (x, sumOfAllDistances x 0 points)) allCoords
    print $ length $ filter (\x -> (snd x) < 10000) allSums

sumOfAllDistances :: Point2d -> Int -> [Point2d] -> Int
sumOfAllDistances x i []         = i
sumOfAllDistances x i (pt : pts) = sumOfAllDistances x (i + Kd.dist2 x pt) pts

getPointString :: M.Map Point2d Char -> Point2d -> Char
getPointString m p = case M.lookup p m of
    Nothing -> '.'
    Just c  -> c

splitArea :: Int -> String -> String
splitArea _ [] = []
splitArea i s  = let (h, t) = splitAt i s in h ++ "\n" ++ (splitArea i t)

readLine :: String -> (Int, Int)
readLine s = (startInt, endInt)
  where
    (start, end) = span isDigit s
    startInt     = read start :: Int
    endInt       = read (drop 2 end) :: Int


incrementCounts
    :: Kd.KdTree Point2d -> M.Map Point2d Int -> [Point2d] -> M.Map Point2d Int
incrementCounts _    m []         = m
incrementCounts tree m (pt : pts) = case nearest of
    Nothing        -> incrementCounts tree m pts
    Just neighbour -> incrementCounts tree (M.insertWith (+) neighbour 1 m) pts
    where nearest = getNearest tree pt


getArea :: Kd.KdTree Point2d -> M.Map Point2d Char -> [Point2d] -> String
getArea _ _ [] = ""
getArea tree m (pt : pts) =
    let val = case getNearest tree pt of
            Nothing -> '.'
            Just p  -> m M.! p
    in  val : (getArea tree m pts)

getNearest :: Kd.KdTree Point2d -> Point2d -> Maybe Point2d
getNearest tree pt | d0 /= d1  = Just n0
                   | otherwise = Nothing
  where
    neighbours = Kd.kNearestNeighbors tree 2 pt
    [n0, n1]   = neighbours
    distances  = map (Kd.dist2 pt) neighbours
    [d0, d1]   = distances

type MinX = Int
type MinY = Int
type MaxX = Int
type MaxY = Int
data Bounds = Bounds MinX MaxX MinY MaxY deriving Show

emptyBounds = Bounds (maxBound :: Int)
                     (minBound :: Int)
                     (maxBound :: Int)
                     (minBound :: Int)

getBounds :: Bounds -> [(Int, Int)] -> Bounds
getBounds b                            []            = b
getBounds (Bounds minx maxx miny maxy) ((x, y) : xs) = getBounds
    (Bounds minx' maxx' miny' maxy')
    xs
  where
    minx' = min x minx
    maxx' = max x maxx
    miny' = min y miny
    maxy' = max y maxy
