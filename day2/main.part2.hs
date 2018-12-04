import           Data.List
import           Data.Maybe

main = do
  content <- readFile "./input.txt"
  let linesOfInput = lines content
  let result       = findValuesLevOne distance linesOfInput
  print result

distance :: String -> String -> Int
distance []       []       = 0
distance (a : as) (b : bs) = (if a == b then 0 else 1) + (distance as bs)

findValuesLevOne :: (String -> String -> Int) -> [String] -> (String, String)
findValuesLevOne f (x : xs) =
  let matched = find (\x' -> (f x x') == 1) xs
  in  if null matched then findValuesLevOne f xs else (fromMaybe [] matched, x)
