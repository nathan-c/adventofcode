import qualified Data.Map.Strict               as Map

main = do
  content <- readFile "./input.txt"
  let linesOfInput = lines content
  let mapFuncs     = [ distance x | x <- linesOfInput ]
  let mapped       = zipWith (\f l -> f l) mapFuncs linesOfInput
  print (mapped)

distance :: String -> String -> Int
distance []       []       = 0
distance (a : as) (b : bs) = (if a == b then 0 else 1) + (distance as bs)
