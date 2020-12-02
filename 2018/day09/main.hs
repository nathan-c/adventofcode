import qualified Data.Map                      as M
import qualified Data.Sequence as S

main = do
    --print $ part1 71307 458
    print $ part1 7130700 458

test = do
    print $ part1 25 9
    print $ part1 1618 10
    print $ part1 7999 13
    print $ part1 1104 17
    print $ part1 6111 21
    print $ part1 5807 30



part1 :: Int -> Int -> Int
part1 a b = maximum $ playGame [1 .. a] 0 (replicate b 0) $ S.fromList [0]

playGame :: [Int] -> Int -> [Int] -> S.Seq Int -> [Int]
playGame [] _ scores _           = scores
playGame (1 : turns) 0 scores _ = playGame turns 1 scores $ S.fromList [0, 1]
playGame (turn : turns) current scores board
    | turn `mod` 23 == 0
    = let l             = S.length board
          playerNo      = turn `mod` (length scores)
          (h1, t1)      = splitAt playerNo scores
          x             = (current - 7) `mod` l
          splitLocation = if x == 0 then l else x
          (h2, t2)      = S.splitAt splitLocation board
          scores'       = h1 ++ ((head t1) + turn + S.index t2 0) : (drop 1 t1)
          board'        = h2 S.>< S.drop 1 t2
      in  playGame turns splitLocation scores' board'
    | otherwise
    = let l             = S.length board
          x             = (current + 2) `mod` l
          splitLocation = if x == 0 then l else x
          (h, t)        = S.splitAt splitLocation board
          board'        = h S.>< (turn S.<| t)
      in  playGame turns splitLocation scores board'
