import qualified Data.Map                      as M
import           Data.Time
import           Data.Maybe
import           Data.List
import           Data.Char

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    content <- readFile "./input.txt"
    let times = sortOn fst [ (readDateTime l, drop 19 l) | l <- lines content ]
    let shifts = splitShifts times
    let guards = M.empty :: M.Map Int (M.Map Int Int)
    let processedShifts = processShifts guards shifts
    let minutesAsleep    = getTotalSleepTimes processedShifts
    let (sleepyGuard, _) = last $ sortOn snd $ M.toList minutesAsleep
    print sleepyGuard
    let (sleepiestMinute, _) =
            last $ sortOn snd $ M.toList (processedShifts M.! sleepyGuard)
    print $ sleepyGuard * sleepiestMinute

part2 :: IO ()
part2 = do
    content <- readFile "./input.txt"
    let times = sortOn fst [ (readDateTime l, drop 19 l) | l <- lines content ]
    let shifts = splitShifts times
    let guards = M.empty :: M.Map Int (M.Map Int Int)
    let processedShifts = processShifts guards shifts
    let maxMinute = M.map (last . (sortOn snd . M.toList))
            $ M.filter (\l -> length l > 0) processedShifts
    let (sleepyGuard, (sleepyMinute, _)) =
            last $ sortOn (snd . snd) $ M.toList maxMinute
    print $ sleepyGuard * sleepyMinute

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

readDateTime :: [Char] -> UTCTime
readDateTime line =
    fromJust
        $ (parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M" (slice 1 16 line) :: Maybe
                UTCTime
          )

getTotalSleepTimes :: M.Map Int (M.Map Int Int) -> M.Map Int Int
getTotalSleepTimes m = M.map sumVals m

sumVals :: M.Map Int Int -> Int
sumVals = sum . (M.elems)

isStart :: (UTCTime, String) -> Bool
isStart (_, s) = "Guard" `isPrefixOf` s

splitShifts :: [(UTCTime, String)] -> [[(UTCTime, String)]]
splitShifts [] = []
splitShifts (x : xs) =
    let (a, b) = break isStart xs in ([x] ++ a) : (splitShifts b)

-- process groups into map per guard containing the count of minutes asleep
-- this method could probably do with some optimizing....
processShifts
    :: M.Map Int (M.Map Int Int)
    -> [[(UTCTime, String)]]
    -> M.Map Int (M.Map Int Int)
processShifts guards [] = guards
processShifts guards [shift] =
    let guardId            = readGuardId $ snd $ shift !! 0
        guardSleepPattern  = M.findWithDefault M.empty guardId guards
        guardSleepPattern' = processShift guardSleepPattern (drop 1 shift)
    in  M.insert guardId guardSleepPattern' guards
processShifts guards (shift : shifts) =
    let guardId            = readGuardId $ snd $ shift !! 0
        guardSleepPattern  = M.findWithDefault M.empty guardId guards
        guardSleepPattern' = processShift guardSleepPattern (drop 1 shift)
    in  processShifts (M.insert guardId guardSleepPattern' guards) shifts


processShift :: M.Map Int Int -> [(UTCTime, String)] -> M.Map Int Int
processShift m [] = m
processShift m [(UTCTime _ t, "falls asleep")] =
    let minute = floor t `div` 60 in insertAll [minute .. 60] m
processShift m [(UTCTime _ t1, "falls asleep"), (UTCTime _ t2, "wakes up")] =
    let m1 = floor t1 `div` 60
        m2 = floor t2 `div` 60
    in  insertAll [m1 .. m2 - 1] m
processShift m ((UTCTime _ t1, "falls asleep") : (UTCTime _ t2, "wakes up") : xs)
    = let m1 = floor t1 `div` 60
          m2 = floor t2 `div` 60
      in  processShift (insertAll [m1 .. m2 - 1] m) xs
processShift m l = error ("Invalid inputs m:" ++ show m ++ ". l:" ++ show l)


readGuardId :: String -> Int
readGuardId x = read (takeWhile isDigit (drop 7 x)) :: Int

insertAll :: [Int] -> M.Map Int Int -> M.Map Int Int
insertAll []       m = m
insertAll [x     ] m = M.insertWith (+) x 1 m
insertAll (x : xs) m = insertAll xs m' where m' = M.insertWith (+) x 1 m
