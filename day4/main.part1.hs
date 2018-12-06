import Data.Time
import Data.Maybe
import Data.List

slice from to xs = take (to - from + 1) (drop from xs)

readDateTime line = fromJust $ (parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M" (slice 1 16 line) :: Maybe UTCTime)

getKey :: (k,v) -> k
getKey (k,_) = k

main = do
    content <- readFile "./input.txt"
    let times = sortOn getKey [(readDateTime l, drop 19 l )| l <- lines content ]
    let groupStarts = drop 1 $ findIndices isStart times
    let shifts = groupGuard (runningGroupStarts groupStarts) times
    print shifts

isStart :: (UTCTime, String) -> Bool
isStart (d,s) = "Guard" `isPrefixOf` s

groupGuard :: [Int] -> [(UTCTime,String)] -> [[(UTCTime,String)]]
groupGuard [] vals = [vals]
groupGuard (i:is) vals = 
    let (x, xs) = splitAt i vals
    in x:(groupGuard is xs)

runningGroupStarts :: [Int] -> [Int]
runningGroupStarts [] = []
runningGroupStarts (x:x':xs) = (x'-x):(runningGroupStarts xs)

-- process groups into map per guard containing the count of minutes asleep

processShift :: [(UTCTime, "")]