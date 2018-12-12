import           Data.List
import qualified Data.Map                      as M

type X = Int
type Y = Int
type SerialNumber = Int

main = part2

part1 :: IO ()
part1 = do
    -- print $ getPowerLevel 8 3 5
    -- print $ getPowerLevel 57 122 79
    -- print $ getPowerLevel 39 217 196
    -- print $ getPowerLevel 71 101 153
    let powers =
            [ ( (x, y)
              , foldl
                  (\agg (x'', y'') -> agg + (getPowerLevel 2568 x'' y''))
                  0
                  [ (x', y') | x' <- [x .. (x + 2)], y' <- [y .. (y + 2)] ]
              )
            | x <- [1 .. 298]
            , y <- [1 .. 298]
            ]
    print $ maximumBy (\a b -> (snd a) `compare` (snd b)) powers

part2 = do
    let powers = flatten
            [ [ ( (x, y, n)
                , foldl'
                    (\agg (x'', y'') -> agg + (getPowerLevel 2568 x'' y''))
                    0
                    [ (x', y') | x' <- [x .. (x + n)], y' <- [y .. (y + n)] ]
                )
              | x <- [1 .. (300 - n)]
              , y <- [1 .. (298 - n)]
              ]
            | n <- [0 .. 299]
            ]
    
    print $ maximumBy (\a b -> (snd a) `compare` (snd b)) powers

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs


getPowerLevel :: SerialNumber -> X -> Y -> Int
getPowerLevel s x y = powerFinal
  where
    rackId     = x + 10
    power1     = rackId * y
    power2     = power1 + s
    power3     = power2 * rackId
    power4     = (power3 `div` 100) `mod` 10
    powerFinal = power4 - 5


