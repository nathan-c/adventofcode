import           Data.List
import qualified Data.Map.Strict               as M

type X = Int
type Y = Int
type SerialNumber = Int

serial = 2568

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
                  (\agg (x'', y'') -> agg + (getPowerLevel serial x'' y''))
                  0
                  [ (x', y') | x' <- [x .. (x + 2)], y' <- [y .. (y + 2)] ]
              )
            | x <- [1 .. 298]
            , y <- [1 .. 298]
            ]
    print $ maximumBy (\a b -> (snd a) `compare` (snd b)) powers

part2 = do
    let rollingSums = buildRollingSums
    let powers = flatten
            [ [ ((x, y, n), readPowerLevel (x, y, n) rollingSums)
              | x <- [1 .. (300 - n + 1)]
              , y <- [1 .. (300 - n + 1)]
              ]
            | n <- [1 .. 300]
            ]

    print $ maximumBy (\a b -> (snd a) `compare` (snd b)) powers

readPowerLevel :: (Int, Int, Int) -> M.Map (Int, Int) Int -> Int
readPowerLevel (1, 1, n) m = m M.! (n, n)
readPowerLevel (1, y, n) m = m M.! (n, y + n') - m M.! (n, y - 1)
    where n' = n - 1
readPowerLevel (x, 1, n) m = m M.! (x + n', n) - m M.! (x - 1, n)
    where n' = n - 1
readPowerLevel (x, y, n) m =
    (m M.! (x + n', y + n'))
        - (m M.! (x - 1, y + n'))
        - (m M.! (x + n', y - 1))
        + (m M.! (x - 1, y - 1))
    where n' = n - 1


flatten :: [[a]] -> [a]
flatten []       = []
flatten (x : xs) = x ++ flatten xs

buildRollingSums :: M.Map (Int, Int) Int
buildRollingSums = foldl (\agg v -> addRollingSum v agg)
                         M.empty
                         [ (x, y) | y <- [1 .. 300], x <- [1 .. 300] ]

addRollingSum :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
addRollingSum k@(x, y) m = M.insert k newVal m
  where
    topLeft = M.findWithDefault 0 (x - 1, y - 1) m
    left    = M.findWithDefault 0 (x - 1, y) m
    top     = M.findWithDefault 0 (x, y - 1) m
    p       = getPowerLevel serial x y
    newVal  = p + top + left - topLeft


getPowerLevel :: SerialNumber -> X -> Y -> Int
getPowerLevel s x y = powerFinal
  where
    rackId     = x + 10
    power1     = rackId * y
    power2     = power1 + s
    power3     = power2 * rackId
    power4     = (power3 `div` 100) `mod` 10
    powerFinal = power4 - 5


