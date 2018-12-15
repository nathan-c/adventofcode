import           Data.List
import           Data.Maybe
import qualified Data.Map                      as M

main :: IO ()
main = part2

_part1 :: IO ()
_part1 = do
    content <- readFile "./input.txt"
    let (carts, tracks) = readInput (0, 0) (lines content) (M.empty, M.empty)
    let aggregate agg _ = sortOn fst $ _moveOneWithCheck [] agg tracks
    let runningSystem = scanl aggregate (M.toAscList carts) [1 ..]
    -- just run the system until it throws. the result will be in the error message (bad practise I know)
    print $ runningSystem

part2 :: IO ()
part2 = do
    content <- readFile "./input.txt"
    let (carts, tracks) = readInput (0, 0) (lines content) (M.empty, M.empty)
    let aggregate agg _ = sortOn fst $ _moveOneAndRemove [] agg tracks
    let runningSystem       = scanl aggregate (M.toAscList carts) [1 ..]
    --print $ take 5 runningSystem
    let (_, (lastCart : _)) = span (\x -> 1 < (length x)) runningSystem
    print $ lastCart


data Direction = North | East | South | West deriving (Show, Read, Eq, Ord, Enum, Bounded)
data Track = Vertical | Horizontal | FwdCorner | BwdCorner | CrossRoad deriving (Show, Eq)
data TurnPref = Lft | Strght | Rght deriving (Show, Eq, Enum, Bounded)

turnPrefToInt :: TurnPref -> Int
turnPrefToInt Lft    = -1
turnPrefToInt Strght = 0
turnPrefToInt Rght   = 1

type Position = (Int,Int)
type Cart = (Direction,TurnPref)
type CartPositions = M.Map Position Cart
type TrackLocations = M.Map Position Track
type System = (CartPositions,TrackLocations)

_testForCollisionInStep :: [[(Position, Cart)]] -> Maybe Position
_testForCollisionInStep (a : b : xs)
    | isNothing possibleCollision = _testForCollisionInStep (b : xs)
    | otherwise                   = possibleCollision
    where possibleCollision = _checkForCollisions (sortOn fst a ++ b)
_testForCollisionInStep _ = Nothing

_checkForCollisions :: [(Position, Cart)] -> Maybe Position
_checkForCollisions ((p1, _) : cart@(p2, _) : carts)
    | p1 == p2  = Just p1
    | otherwise = _checkForCollisions (cart : carts)
_checkForCollisions _ = Nothing

_moveOne :: [(Position, Cart)] -> TrackLocations -> [(Position, Cart)]
_moveOne [] _ = []
_moveOne (cart : carts) tracks =
    (_moveSingleCart cart tracks) : _moveOne carts tracks

_moveOneWithCheck
    :: [(Position, Cart)]
    -> [(Position, Cart)]
    -> TrackLocations
    -> [(Position, Cart)]
_moveOneWithCheck _ [] _ = []
_moveOneWithCheck alreadyMoved (cart : carts) tracks
    | isNothing test = moved : _moveOneWithCheck alreadyMoved' carts tracks
    | otherwise      = error $ "collision at " ++ show moved
  where
    moved         = _moveSingleCart cart tracks
    alreadyMoved' = alreadyMoved ++ [moved]
    test          = _checkForCollisions (sortOn fst (alreadyMoved' ++ carts))


_moveOneAndRemove
    :: [(Position, Cart)]
    -> [(Position, Cart)]
    -> TrackLocations
    -> [(Position, Cart)]
_moveOneAndRemove alreadyMoved []             _      = alreadyMoved
_moveOneAndRemove alreadyMoved (cart : carts) tracks = _moveOneAndRemove
    alreadyMoved''
    carts'
    tracks
  where
    moved          = _moveSingleCart cart tracks
    alreadyMoved'  = sortOn fst (alreadyMoved ++ [moved])
    test           = _checkForCollisions (sortOn fst (alreadyMoved' ++ carts))
    alreadyMoved'' = case test of
        Nothing -> alreadyMoved'
        Just p  -> _remove p alreadyMoved'
    carts' = case test of
        Nothing -> carts
        Just p  -> _remove p carts

_remove :: Position -> [(Position, Cart)] -> [(Position, Cart)]
_remove _ [] = []
_remove p1 l@(cart@(p2, _) : carts) | p1 == p2  = _remove p1 carts
                                    | p1 < p2   = l
                                    | otherwise = cart : _remove p1 carts

_newDirection :: Cart -> Track -> Cart
_newDirection (North, turnPref) FwdCorner = (East, turnPref)
_newDirection (East , turnPref) FwdCorner = (North, turnPref)
_newDirection (South, turnPref) FwdCorner = (West, turnPref)
_newDirection (West , turnPref) FwdCorner = (South, turnPref)
_newDirection (North, turnPref) BwdCorner = (West, turnPref)
_newDirection (West , turnPref) BwdCorner = (North, turnPref)
_newDirection (South, turnPref) BwdCorner = (East, turnPref)
_newDirection (East , turnPref) BwdCorner = (South, turnPref)
_newDirection (d, turnPref) CrossRoad =
    (cyclicAdd d $ turnPrefToInt turnPref, cyclicSucc turnPref)
_newDirection (d, turnPref) _ = (d, turnPref)

_newPosition :: Position -> Direction -> Position
_newPosition (x, y) North = (x, y - 1)
_newPosition (x, y) South = (x, y + 1)
_newPosition (x, y) East  = (x + 1, y)
_newPosition (x, y) West  = (x - 1, y)


_moveSingleCart :: (Position, Cart) -> TrackLocations -> (Position, Cart)
_moveSingleCart (position, cart@(d, _)) tracks = (position', direction')
  where
    position'  = _newPosition position d
    nextTrack  = tracks M.! position'
    direction' = _newDirection cart nextTrack


-- Input parsing logic below here

readInput :: Position -> [String] -> System -> System
readInput _        []       system = system
readInput p@(x, y) (l : ls) system = readInput (x, y + 1) ls system'
    where system' = readLine p l system

readTrack :: Position -> CartPositions -> TrackLocations -> Track -> System
readTrack p carts tracks track =
    let tracks' = M.insert p track tracks in (carts, tracks')

readCart :: Position -> CartPositions -> TrackLocations -> Direction -> System
readCart p carts tracks direction =
    let carts'  = M.insert p (direction, Lft) carts
        tracks' = M.insert
            p
            (if direction == North || direction == South
                then Vertical
                else Horizontal
            )
            tracks
    in  (carts', tracks')

readLine :: Position -> String -> System -> System
readLine _      []         output = output
readLine (x, y) (' ' : cs) output = readLine (x + 1, y) cs output
readLine p@(x, y) ('|' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readTrack p carts tracks Vertical
readLine p@(x, y) ('-' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readTrack p carts tracks Horizontal
readLine p@(x, y) ('\\' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readTrack p carts tracks BwdCorner
readLine p@(x, y) ('/' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readTrack p carts tracks FwdCorner
readLine p@(x, y) ('+' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readTrack p carts tracks CrossRoad
readLine p@(x, y) ('<' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readCart p carts tracks West
readLine p@(x, y) ('>' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readCart p carts tracks East
readLine p@(x, y) ('^' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readCart p carts tracks North
readLine p@(x, y) ('v' : cs) (carts, tracks) =
    readLine (x + 1, y) cs $ readCart p carts tracks South
readLine p (c : _) _ = error $ "cannot parse: " ++ show c ++ " at " ++ show p

-- |
-- >>> take 10 $ iterate cyclicSucc Red
-- [Red,Yellow,Green,Cyan,Blue,Magenta,Red,Yellow,Green,Cyan]
cyclicSucc :: (Enum a, Bounded a, Eq a) => a -> a
cyclicSucc x | x == maxBound = minBound
             | otherwise     = succ x

cyclicPred :: (Enum a, Bounded a, Eq a) => a -> a
cyclicPred x | x == minBound = maxBound
             | otherwise     = pred x

cyclicAdd :: (Enum a, Bounded a, Eq a) => a -> Int -> a
cyclicAdd x 0 = x
cyclicAdd x i | i > 0     = foldl (\agg _ -> cyclicSucc agg) x [1 .. i]
              | otherwise = foldl (\agg _ -> cyclicPred agg) x [1 .. (-i)]
