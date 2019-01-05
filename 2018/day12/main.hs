import           Data.Foldable                  ( toList )
import qualified Data.Sequence                 as S
import qualified Data.IntSet                   as I

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    initialStateFile <- readFile "./initial_state.txt"
    let initialState = [ parseFromInput x | x <- initialStateFile ]
    inputFile <- readFile "./input.txt"
    let tests =
            [ ([ parseFromInput x | x <- take 5 l ], parseFromInput $ last l)
            | l <- lines inputFile
            ]

    let firstGen =
            moveGeneration tests ([False, False] ++ initialState) S.empty
    --print initialState
    --print firstGen
    let n = 20
    let aggregator agg _ =
            moveGeneration tests ([False, False, False] ++ (toList agg)) S.empty
    let nthGen = foldl aggregator (S.fromList initialState) [1 .. n]

    --print $ easyPrint $ toList nthGen
    let val =
            sum $ zipWith (\a b -> if b then a else 0) [(-n) ..] $ toList nthGen
    print val

part2 :: IO ()
part2 = do
    initialStateFile <- readFile "./initial_state.txt"
    let initialState = [ parseFromInput x | x <- initialStateFile ]
    inputFile <- readFile "./input.txt"
    let tests =
            [ ([ parseFromInput x | x <- take 5 l ], parseFromInput $ last l)
            | l <- lines inputFile
            ]
    let state  = I.fromList [ i | (on, i) <- zip initialState [0 ..], on ]
    let getBounds s = ((I.findMin s) - 3, (I.findMax s) + 2)
    let n = 50000000000
    --      2600000001872 Cheated a bit here as I noticed a pattern by running 500, 5000, 50000 etc
    let aggregator agg _ = moveGeneration2 (getBounds agg) tests agg I.empty

    let nthGen                           = foldl aggregator state [1 .. n]
    let finalBounds@(finalMin, finalMax) = getBounds nthGen
    print finalBounds
    print $ sum $ filter (\a -> a `I.member` nthGen) [finalMin .. finalMax] 

type Test = ([Bool], Bool)

testAllSurvivalGuides :: [Bool] -> [Test] -> Bool
testAllSurvivalGuides _ [] = False
testAllSurvivalGuides slice ((test, val) : tests)
    | (take 5 (slice ++ repeat False)) == test = val
    | otherwise = testAllSurvivalGuides slice tests

moveGeneration :: [Test] -> [Bool] -> S.Seq Bool -> S.Seq Bool
moveGeneration _     []          next = next
moveGeneration tests (t0 : this) next = moveGeneration tests (this) next'
    where next' = next S.|> (testAllSurvivalGuides (t0 : this) tests)

type Bounds = (Int,Int)

moveGeneration2 :: Bounds -> [Test] -> I.IntSet -> I.IntSet -> I.IntSet
moveGeneration2 (minI, maxI) tests current next
    | minI < maxI = moveGeneration2 (minI + 1, maxI) tests current next'
    | otherwise   = next
  where
    nextL = [ i `I.member` current | i <- [minI .. maxI] ]
    on    = testAllSurvivalGuides nextL tests
    next' = if on then I.insert (minI + 2) next else next


parseFromInput :: Char -> Bool
parseFromInput '#' = True
parseFromInput '.' = False
parseFromInput t   = error $ "unknown input " ++ show t

splitAtPad :: Int -> [Bool] -> ([Bool], [Bool])
splitAtPad i xs | h == []   = (h, t)
                | t == []   = (h ++ replicate (i - length h) False, t)
                | otherwise = (h, t)
    where (h, t) = splitAt i xs

easyPrint :: [Bool] -> String
easyPrint []           = ""
easyPrint (True  : xs) = "#" ++ easyPrint xs
easyPrint (False : xs) = "." ++ easyPrint xs

