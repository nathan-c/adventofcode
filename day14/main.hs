import qualified Data.Sequence                 as S
import           Data.Char
import           Data.Foldable

main :: IO ()
main = do
    let start = S.fromList [3, 7]
    -- let second = runOneRound start 0 1
    -- print second
    -- print $ pickRecipe second 0
    -- print $ pickRecipe second 1
    -- let third = runOneRound second (pickRecipe second 0) (pickRecipe second 1)
    let n     = 074501
    let (x, _, _) = foldl
            (\(recipes, i1, i2) _ ->
                let recipes' = runOneRound recipes i1 i2
                in  (recipes', pickRecipe recipes' i1, pickRecipe recipes' i2)
            )
            (start, 0, 1)
            [1 .. (n * 500)] 
            -- a length of n*500 is enough to get the answer but used over 12GB RAM.... Im sure this could be reduced with a lttle effort.
    print $ map intToDigit $ toList $ S.take 10 $ S.drop n x
    print $ findPuzzle 0 (splitInt 6 n) x

findPuzzle :: Int -> [Int] -> S.Seq Int -> Int
findPuzzle i test recipes
    | S.length recipes == 0 = error $ "never found sequence " ++ show test
    | h == test             = i
    | otherwise             = findPuzzle (i + 1) test (S.drop 1 recipes)
    where h = toList $ S.take (length test) recipes

splitInt :: Int -> Int -> [Int]
splitInt len i = reverse (take len ((reverse $ _splitInt i) ++ repeat 0))

_splitInt :: Int -> [Int]
_splitInt 0 = []
_splitInt i = (_splitInt div10) ++ [mod10]
  where
    mod10 = i `mod` 10
    div10 = i `div` 10

runOneRound :: S.Seq Int -> Int -> Int -> S.Seq Int
runOneRound recipes i1 i2 = recipes S.>< (S.fromList newRecipes)
  where
    r1         = recipes `S.index` i1
    r2         = recipes `S.index` i2
    newRecipes = makeHotChocRecipes r1 r2


pickRecipe :: S.Seq Int -> Int -> Int
pickRecipe recipes currentI = nextI
  where
    currentScore = recipes `S.index` currentI
    nextI        = (currentI + currentScore + 1) `mod` S.length recipes


makeHotChocRecipes :: Int -> Int -> [Int]
makeHotChocRecipes a b = cs
  where
    c  = a + b
    cs = if c > 9 then [1, c - 10] else [c]
