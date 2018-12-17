{-# LANGUAGE DeriveGeneric #-}

import           Data.Graph.AStar
import qualified Data.HashSet                  as H
import qualified Data.Map.Strict               as M
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Ord
import           GHC.Generics                   ( Generic )


main = do
    content <- readFile "./test.txt"
    let input = readInput (Location 0 0) (lines content) []
    print input
    runGame $ input

runGame :: [Unit] -> IO ()
runGame units = do
    let actualUnits = filter (\(_, (t, _)) -> t == Elf || t == Goblin) units
    let units'      = runRound actualUnits units
    putStrLn $ showAll 0 units'
    let goblins  = any (\(_, (t, _)) -> t == Goblin) units'
    let elves    = any (\(_, (t, _)) -> t == Elf) units'
    let finished = goblins `xor` elves
    if finished then print "GAME OVER" else runGame units'


runRound :: [Unit] -> [Unit] -> [Unit]
runRound [] allUnits = allUnits
runRound (movableUnit : movableUnits) allUnits =
    let
        targets              = findTargets movableUnit allUnits
        --inRangeTargets = filter (movableUnit isInRange) targets 
        (attacked, targets') = attackIfYouCan movableUnit targets
    in
        if attacked
            then
                let (movableUnits', allUnits') =
                        processAttack targets' (movableUnits, allUnits)
                in  runRound movableUnits' allUnits'
            else
                let
                    targetSquares = map fst $ flatten
                        $ map (\t -> identifyOpenSquares t allUnits) targets
                    targetSquaresSet = H.fromList targetSquares
                    shortestPath = closestInRangeSquare targetSquaresSet movableUnit targets
                in
                    if isNothing shortestPath
                        then runRound movableUnits allUnits
                        else
                            let
                                Just (firstStep : _) = shortestPath
                                allUnits' = processMovement movableUnit
                                                            firstStep
                                                            allUnits
                                (attacked, targets'') = attackIfYouCan
                                    movableUnit
                                    (findTargets movableUnit allUnits')
                            in
                                if attacked
                                    then
                                        let
                                            (movableUnits', allUnits'') =
                                                processAttack
                                                    targets''
                                                    (movableUnits, allUnits')
                                        in  runRound movableUnits' allUnits''
                                    else runRound movableUnits allUnits'

processAttack targets (movableUnits, allUnits) =
    let targetMap     = M.fromList targets
        movableUnits' = M.toList $ M.union targetMap (M.fromList movableUnits)
        allUnits'     = M.toList $ M.union targetMap (M.fromList allUnits)
    in  (movableUnits', allUnits')

processMovement :: Unit -> Location -> [Unit] -> [Unit]
processMovement unit location allUnits =
    let unitMap          = M.fromList allUnits
        (oldLocation, x) = unit
    in  M.toList $ M.insert location
                            x
                            (M.insert oldLocation (Cavern, 200) unitMap)

attackIfYouCan :: Unit -> [Unit] -> (Bool, [Unit])
attackIfYouCan attacker [] = (False, [])
attackIfYouCan attacker (defender : defenders) = if isInRange attacker defender
    then case attack defender of
        Nothing -> (True, defenders)
        Just x  -> (True, x : defenders)
    else
        let (success, end) = (attackIfYouCan attacker defenders)
        in  (success, defender : end)


data Location = Location {y::Int, x::Int} deriving (Eq, Ord, Generic, Show)
type HP = Int
type Unit = (Location, (UnitType, HP))
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
findTargets unit@(_, (t1, _)) (other@(_, (t2, _)) : others)
    | t2 == t1  = findTargets unit others
    | otherwise = other : findTargets unit others

dist :: Unit -> Unit -> Int
dist (Location y1 x1, (_, _)) (Location y2 x2, (_, _)) =
    abs (y1 - y2) + abs (x1 - x2)

distL :: Location -> Unit -> Int
distL (Location y1 x1) (Location y2 x2, (_, _)) = abs (y1 - y2) + abs (x1 - x2)

isInRange :: Unit -> Unit -> Bool
isInRange a b = dist a b == 1

identifyOpenSquares :: Unit -> [Unit] -> [Unit]
identifyOpenSquares unit [] = []
identifyOpenSquares unit (square@(_, (Cavern, _)) : squares)
    | (dist unit square) == 1 = square : identifyOpenSquares unit squares
    | otherwise               = identifyOpenSquares unit squares
identifyOpenSquares unit (_ : squares) = identifyOpenSquares unit squares

attack :: Unit -> Maybe Unit
attack defender@(l, (t, hp)) | hp > 3    = Just (l, (t, hp - 3))
                             | otherwise = Nothing

closestInRangeSquare :: H.HashSet Location -> Unit -> [Unit] -> Maybe [Location]
closestInRangeSquare possiblePositions start possibleEnds =
    if null shortestRoute then Nothing else Just shortestRoute
  where
    possibleRoutes = map (shortestPath possiblePositions start) possibleEnds
    shortestRoute  = minimumBy (comparing length) (catMaybes possibleRoutes)

shortestPath :: H.HashSet Location -> Unit -> Unit -> Maybe [Location]
shortestPath possiblePositions start@(startL, (_, _)) possibleEnd@(endL, (_, _))
    = aStar getNeighbours getDistance minimumDistance isGoal startL
  where
    getNeighbours (Location y x) = H.intersection
        (H.fromList [ Location y (x - 1), Location y (x + 1), Location (y - 1) x, Location (y + 1) x])
        possiblePositions
    getDistance _ _ = 1
    minimumDistance a = distL a possibleEnd
    isGoal a = a == endL

move :: Unit -> Location -> Unit
move (_, (t, hp)) location = (location, (t, hp))


readInput :: Location -> [String] -> [Unit] -> [Unit]
readInput _                  []             units = units
readInput loc@(Location y x) (line : lines) units = readInput (Location (y + 1) x) lines units'
    where 
        units' = readLine loc line units

readLine :: Location -> String -> [Unit] -> [Unit]
readLine loc@(Location y x) line units = units ++ newUnits
  where
    unitTypes = map (\char -> read [char] :: UnitType) line
    newUnits =
        zip (map (\i -> Location y i) [x ..]) $ zip unitTypes (repeat 200)

showAll :: Int -> [Unit] -> String
showAll _    []    = []
showAll line units = thisString ++ "\n" ++ nextString
  where
    (this, next) = span (\(Location y _, (_, _)) -> y == line) units
    thisString   = map (\(_, (u, _)) -> (show u) !! 0) this
    nextString   = showAll (line + 1) next

xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False
