import qualified Data.MultiMap                      as M
import Data.List

data Point2d = Point2d Int Int deriving (Show, Eq, Ord)
type Velocity = (Int,Int)
data Point = Point {position ::( Int,Int), velocity :: (Int,Int)} deriving (Show, Read)

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let startingPoints = [ read l :: Point | l <- lines content ]
    let pointsAndVelocities = [ (Point2d x y, v) | Point (x, y) v <- startingPoints ]
    --M.keys $ moveOne positions
    let kdm   = M.fromList pointsAndVelocities
    let (moved, i) = runPart1 0 kdm

    print $ [ (x, y) | Point2d x y <- M.keys $ moved ]
    print i

checkAll :: [Point2d] -> M.MultiMap Point2d Velocity -> Bool
checkAll [] _ = True
checkAll (p : ps) m | isNextTo  = checkAll ps m
                    | otherwise = False
  where
    nil                = (0, 0)
    Point2d x y        = p
    expectedNeighbours = 
        [ (Point2d x' y', nil)
        | y' <- [(y - 1) .. (y + 1)]
        , x' <- [(x - 1) .. (x + 1)]
        , (x', y') /= (x, y)
        ]
    isNextTo = not $ null $ find ((M.member m).fst) expectedNeighbours

moveOne :: M.MultiMap Point2d Velocity -> M.MultiMap Point2d Velocity
moveOne m = M.fromList points'
  where
    points = M.toList m
    points' = [ (Point2d (x + vx) (y + vy), vel)
              | (Point2d x y, vel@(vx, vy)) <- points
              ]

runPart1 :: Int -> M.MultiMap Point2d Velocity -> (M.MultiMap Point2d Velocity, Int)
runPart1 i m | checkAll (M.keys m) m = (m, i)
             | otherwise             = runPart1 (i+1) $ moveOne m
