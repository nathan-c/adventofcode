import           Data.List.Split

main = do
    content <- readFile "./input.txt"
    let treeEnc     = map (\x -> read x :: Int) $ splitOn " " content
    --print treeEnc
    --print $ sizeOfNode treeEnc
    let (tree, _)   = buildTree treeEnc
    --print tree
    let allMetadata = sumMetadata 0 tree
    print allMetadata
    let p2 = sumNodes 0 tree
    print p2


data TreeNode = TreeNode [TreeNode] [Int] deriving (Show)

buildTree :: [Int] -> (TreeNode, [Int])
buildTree (0 : m : xs) = (TreeNode [] meta, xs')
    where (meta, xs') = splitAt m xs
buildTree (n : m : xs) = (TreeNode children meta, xs')
  where
    (children, tail) = foldl
        (\(i, t) _ -> let (i', t') = buildTree t in (i ++ [i'], t'))
        ([], xs)
        [1 .. n]
    (meta, xs') = splitAt m tail


sizeOfNode :: [Int] -> (Int, [Int])
sizeOfNode []           = (0, [])
sizeOfNode (0 : m : xs) = (2 + m, drop m xs)
sizeOfNode (n : m : xs) = (2 + m + sumz, tail)
  where
    (sumz, tail) = foldl
        (\(i, tail) _ -> let (i', tail') = sizeOfNode tail in (i + i', tail'))
        (0, xs)
        [1 .. n]
sizeOfNode x = error $ "Missed a pattern: " ++ show x

sumMetadata :: Int -> TreeNode -> Int
sumMetadata i (TreeNode [] meta) = i + sum meta
sumMetadata i (TreeNode children meta) =
    i + (foldl (\agg x -> agg + sumMetadata 0 x) 0 children) + sum meta

sumNodes :: Int -> TreeNode -> Int
sumNodes i (TreeNode [] meta) = i + sum meta
sumNodes i (TreeNode children metadata) =
    i + sum [ sumNodes 0 (children !! (m-1)) | m <- metadata, m > 0 && m <= length children ]
