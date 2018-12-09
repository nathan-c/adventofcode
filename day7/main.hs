import           Data.List
import           Data.Tuple
import           Data.List.Unique
import           Data.Char
import qualified Data.Map                      as M


main :: IO ()
main = part1

part1 :: IO ()
part1 = do
    content <- readFile "./input.txt"
    let relations    = [ readLine x | x <- lines content ]
    let graph        = buildGraph M.empty relations
    let reverseGraph = buildGraph M.empty $ map swap relations
    let allNodes =
            sortUniq
                $       [ x | (x, _) <- relations ]
                `union` [ x | (_, x) <- relations ]
    let roots = sort $ allNodes \\ M.keys reverseGraph
    let order = traverseGraph roots "" graph reverseGraph
    print order
    
    print $ processSecond 0 (map (\x->(x, getStepTime x)) roots,[]) graph reverseGraph ""



aOffset = ord 'A'
maxInParallel = 10

getStepTime :: Char -> Int
getStepTime node = 60 + intVal - aOffset + 1 where intVal = ord node

processSecond :: Int -> ([(Char, Int)],[(Char, Int)]) -> M.Map Char [Char] -> M.Map Char [Char] -> String -> Int
processSecond i ([],[]) _ _ _ = i
processSecond i (inProcess,queued) graph reverseGraph processed =
    let 
        inProcess'               = map (\(x, i) -> (x, i - 1)) inProcess
        (finished, stillWorking) = partition ((== 0) . snd) inProcess'
        newItems                 = flatten $ map
            (\x -> map (\y -> (y, getStepTime y)) $ M.findWithDefault [] x graph)
            $ map fst finished
        processed' = processed ++ map fst finished
        newItems' = filter ((shouldRemove reverseGraph processed').fst) newItems
        queued' = sortOn fst (newItems' ++ queued)
        numberToPickup = if (length stillWorking) >= maxInParallel then 0 else maxInParallel - (length stillWorking)
        (newItems'', queued'') = splitAt numberToPickup queued'
        inProcess'' = (stillWorking ++ newItems'')
    in  processSecond (i + 1) (inProcess'',queued'') graph reverseGraph processed'

readLine :: String -> (Char, Char)
readLine l = (l !! 5, l !! 36)

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

buildGraph :: M.Map Char [Char] -> [(Char, Char)] -> M.Map Char [Char]
buildGraph m [] = m
buildGraph m ((parent, child) : rs) =
    buildGraph (M.insertWith (++) parent [child] m) rs

traverseGraph
    :: [Char] -> String -> M.Map Char [Char] -> M.Map Char [Char] -> String
traverseGraph []       result _     _            = result
traverseGraph (n : ns) result graph reverseGraph = traverseGraph
    ns'
    result'
    graph
    reverseGraph
  where
    result'          = result ++ [n]
    children         = M.findWithDefault [] n graph
    filteredChildren = filter (shouldRemove reverseGraph result') children
    ns'              = sort $ filteredChildren `union` ns

shouldRemove :: M.Map Char [Char] -> String -> Char -> Bool
shouldRemove reverseGraph result x =
    let parents = reverseGraph M.! x in all (`elem` result) parents
