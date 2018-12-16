
main = do
    print "hello"

data Location = Location {y::Int, x::Int} deriving (Eq, Ord)
type HP = Int
type Unit = (UnitType, Location, HP)
strength = 3
startingHp = 200
data UnitType = Elf | Goblin | Wall | Cavern deriving (Eq)

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


findTargets :: Unit -> [Unit] -> [Unit]
findTargets _ [] = []
findTargets unit@(t1, _, _) (other@(t2, _, _) : others)
    | t2 == t1  = findTargets unit others
    | otherwise = other : findTargets unit others

dist :: Unit -> Unit -> Int
dist (_, Location y1 x1, _) (_, Location y2 x2, _) =
    abs (y1 - y2) + abs (x1 - x2)

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
