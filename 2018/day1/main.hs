import qualified Data.Set                      as Set


main = do
  content <- readFile "./input.txt"
  let linesOfFiles = [ read x :: Integer | x <- lines content ]
  let seenList     = tail (runningSum 0 linesOfFiles)
  let seen         = Set.fromList seenList
  let xs = (tail (runningSum (last seenList) (cycle linesOfFiles)))
  print (getAnswer seen xs)

runningSum :: (Num a) => a -> [a] -> [a]
runningSum i []       = [i]
runningSum i (x : xs) = i : runningSum (i + x) xs

getAnswer :: (Ord a) => Set.Set a -> [a] -> a
getAnswer set [] = error "fail"
getAnswer set (x : xs) | Set.member x set = x
                       | otherwise        = getAnswer set xs
