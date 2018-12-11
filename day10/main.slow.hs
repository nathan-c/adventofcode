import qualified Data.KdMap.Static             as K

-- kd tree def
data Point2d = Point2d { x :: Int
                       , y :: Int
                       } deriving Show

point2dAsList :: Point2d -> [Int]
point2dAsList (Point2d x y) = [x, y]

-- / kd tree def

type Velocity = (Int,Int)
data Point = Point {position ::( Int,Int), velocity :: (Int,Int)} deriving (Show, Read)

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let startingPoints = [ read l :: Point | l <- lines content ]
    let pointsAndVelocities =
            [ (Point2d x y, v) | Point (x, y) v <- startingPoints ]
    --M.keys $ moveOne positions
    let kdm   = K.build point2dAsList pointsAndVelocities
    let moved = runPart1 kdm

    print $ [ (x, y) | Point2d x y <- K.keys $ moved ]

checkAll :: [Point2d] -> K.KdMap Int Point2d Velocity -> Bool
checkAll [] _ = True
checkAll (p : ps) m | isNextTo  = checkAll ps m
                    | otherwise = False
  where
    neighbours = K.kNearest m 2 p
    test (Point2d x y) (Point2d x' y') =
        let mhdist = abs (x - x') + abs (y - y') in 1 <= mhdist && mhdist <= 2
    isNextTo = any ((test p) . fst) neighbours

moveOne :: K.KdMap Int Point2d Velocity -> K.KdMap Int Point2d Velocity
moveOne m = K.build point2dAsList points'
  where
    points  = K.assocs m
    points' = map
        (\(Point2d x y, (vx, vy)) -> (Point2d (x + vx) (y + vy), (vx, vy)))
        points

runPart1 :: K.KdMap Int Point2d Velocity -> K.KdMap Int Point2d Velocity
runPart1 m | checkAll (K.keys m) m = m
           | otherwise             = runPart1 $ moveOne m
