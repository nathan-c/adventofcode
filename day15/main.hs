{-# LANGUAGE DeriveGeneric #-}

import           Data.Graph.AStar
import qualified Data.HashSet                  as H
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Ord
import           GHC.Generics                   ( Generic )

main = do
    content <- readFile "./test.txt"
    let units = sortOn (\(_,l,_)->l) $ readInput (Location 0 0) (lines content) []
    putStrLn $ showAll 0 units
    let actualUnits@(h:t) = filter (\(t,_,_) -> t == Elf || t == Goblin) units
    print actualUnits
    let targets = flatten $ map (\t -> identifyOpenSquares t units) $ findTargets h t
    let openSquares = H.fromList $ map (\(_,l,_)->l) $ filter (\(t,_,_) -> t == Cavern || t == Goblin) units
    let Just (l:path) = closestInRangeSquare openSquares h targets
    print l
    putStrLn $ showAll 0 $ insertBy (\(_,l1,_) (_,l2,_) -> l1 `compare` l2) (move h l) (delete h units)

data Location = Location {y::Int, x::Int} deriving (Eq, Ord, Generic, Show)
type HP = Int
type Unit = (UnitType, Location, HP)
strength = 3
startingHp = 200
data UnitType = Elf | Goblin | Wall | Cavern deriving (Eq)

instance Hashable Location

instance Show UnitType where
    show Elf = "E"
    show Goblin = "G"
    show Wall = "#"
    show Cavern = "."

instance Read UnitType where
    readsPrec _ input =
        let ([unitChar],rest) = splitAt 1 input
            unit = _r unitChar
        in case unit of
            Nothing -> []
            Just x -> [(x,rest)]

_r :: Char -> Maybe UnitType
_r 'E' = Just Elf
_r 'G' = Just Goblin
_r '#' = Just Wall
_r '.' = Just Cavern
_r _   = Nothing

flatten :: [[a]] -> [a]
flatten []       = []
flatten (x : xs) = x ++ flatten xs

findTargets :: Unit -> [Unit] -> [Unit]
findTargets _ [] = []
findTargets unit@(t1, _, _) (other@(t2, _, _) : others)
    | t2 == t1  = findTargets unit others
    | otherwise = other : findTargets unit others

dist :: Unit -> Unit -> Int
dist (_, Location y1 x1, _) (_, Location y2 x2, _) =
    abs (y1 - y2) + abs (x1 - x2)

distL :: Location -> Unit -> Int
distL (Location y1 x1) (_, Location y2 x2, _) = abs (y1 - y2) + abs (x1 - x2)

isInRange :: Unit -> Unit -> Bool
isInRange a b = dist a b == 1

identifyOpenSquares :: Unit -> [Unit] -> [Unit]
identifyOpenSquares unit [] = []
identifyOpenSquares unit (square@(Cavern, _, _) : squares)
    | (dist unit square) == 1 = square : identifyOpenSquares unit squares
    | otherwise               = identifyOpenSquares unit squares
identifyOpenSquares unit (_ : squares) = identifyOpenSquares unit squares

attack :: Unit -> Maybe Unit
attack defender@(t, l, hp) | hp > 3    = Just (t, l, hp - 3)
                           | otherwise = Nothing

closestInRangeSquare :: H.HashSet Location -> Unit -> [Unit] -> Maybe [Location]
closestInRangeSquare possiblePositions start possibleEnds =
    if null shortestRoute then Nothing else Just shortestRoute
  where
    possibleRoutes = map (shortestPath possiblePositions start) possibleEnds
    shortestRoute  = minimumBy (comparing length) (catMaybes possibleRoutes)

shortestPath :: H.HashSet Location -> Unit -> Unit -> Maybe [Location]
shortestPath possiblePositions start@(_, startL, _) possibleEnd@(_, endL, _) =
    aStar getNeighbours getDistance minimumDistance isGoal startL
  where
    getNeighbours (Location y x) = H.intersection
        (H.fromList
            [ Location y       (x - 1)
            , Location y       (x + 1)
            , Location (y - 1) x
            , Location (y + 1) x
            ]
        )
        possiblePositions
    getDistance _ _ = 1
    minimumDistance a = distL a possibleEnd
    isGoal a = a == endL

move :: Unit -> Location -> Unit
move (t,_,hp) location = (t,location,hp)


readInput :: Location -> [String] -> [Unit] -> [Unit]
readInput _                  []             units = units
readInput loc@(Location y x) (line : lines) units = readInput
    (Location (y + 1) x)
    lines
    units'
    where units' = readLine loc line units

readLine :: Location -> String -> [Unit] -> [Unit]
readLine loc@(Location y x) line units = units ++ newUnits
  where
    unitTypes = map (\char -> read [char] :: UnitType) line
    newUnits     = zip3 unitTypes (map (\i -> Location y i) [x..]) (repeat 200)

showAll :: Int -> [Unit] -> String
showAll _ [] = []
showAll line units = thisString ++ "\n" ++ nextString
    where
        (this,next) = span (\(_,Location y _,_) -> y == line) units
        thisString = map (\(u,_,_) -> (show u)!!0) this
        nextString = showAll (line+1) next

