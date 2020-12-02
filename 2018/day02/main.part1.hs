import qualified Data.Map.Strict               as Map

main = do
  content <- readFile "./input.txt"
  let linesOfFiles =
        [ getPairsAndTriples x :: (Int, Int) | x <- lines content ]
  let (pairs, triples) =
        foldl (\(a, b) (a', b') -> (a + a', b + b')) (0, 0) linesOfFiles
  print (pairs * triples)


getPairsAndTriples :: String -> (Int, Int)
getPairsAndTriples word =
  ( if Map.null (Map.filter (== 2) m) then 0 else 1
  , if Map.null (Map.filter (== 3) m) then 0 else 1
  )
  where m = getCounts Map.empty word

getCounts :: (Ord a, Num b) => Map.Map a b -> [a] -> Map.Map a b
getCounts m []       = m
getCounts m (x : xs) = getCounts (Map.insertWith (+) x 1 m) xs
