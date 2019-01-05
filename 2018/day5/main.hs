import           Data.Char

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    content <- readFile "./input.txt"
    let processed = processAll content
    print processed
    print $ length processed

part2 :: IO ()
part2 = do
    content <- readFile "./input.txt"
    let filtered = map (\a -> filter (\b -> (toLower b) /= a) content) ['a'..'z']
    --print filtered
    let processed = map (length.processAll) filtered
    --print processed
    print $ minimum processed

processAll :: [Char] -> [Char]
processAll a | l == l'   = a'
             | otherwise = processAll a'
  where
    a' = process a
    l  = length a
    l' = length a'

process :: [Char] -> [Char]
process []  = []
process [a] = [a]
process [a, b] | reacts a b = []
               | otherwise  = [a, b]
process (a : b : xs) | reacts a b = process xs
                     | otherwise  = a: process (b:xs)


reacts :: Char -> Char -> Bool
reacts a b | (isLower a) && (isUpper b) = a == (toLower b)
           | (isUpper a) && (isLower b) = a == (toUpper b)
           | otherwise                  = False

