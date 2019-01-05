import qualified Data.Vector                   as V
import           Data.Bits

allOperationStrings =
    [ "addr"
    , "addi"
    , "mulr"
    , "muli"
    , "banr"
    , "bani"
    , "borr"
    , "bori"
    , "setr"
    , "seti"
    , "gtir"
    , "gtri"
    , "gtrr"
    , "eqir"
    , "eqri"
    , "eqrr"
    ]

main = do
    content1 <- readFile "input1.txt"
    let input = readInput $ lines content1
    print $ part1 input
    let decodedOps = part2 input (V.replicate 16 "")
    print decodedOps
    content2 <- readFile "input2.txt"
    let program = readProgram decodedOps $ lines content2
    let result = runProgram (V.fromList [0,0,0,0]) program
    print result


runOp :: V.Vector Int -> String -> Int -> Int -> Int -> V.Vector Int
runOp registers opCode a b c
    | shortOp == "eq" || shortOp == "gt"
    = let [_, _, ta, tb] = opCode
          a'             = case ta of
              'r' -> registers V.! a
              'i' -> a
          b' = case tb of
              'r' -> registers V.! b
              'i' -> b
      in  registers V.// [(c, a' `op` b')]
    | otherwise
    = let a' = if opString == "set"
              then case last opCode of
                  'r' -> registers V.! a
                  'i' -> a
              else registers V.! a
          b' = case last opCode of
              'r' -> registers V.! b
              'i' -> b
      in  registers V.// [(c, a' `op` b')]
  where
    opString = take 3 opCode
    op       = operation opString
    shortOp  = take 2 opCode



operation :: String -> Int -> Int -> Int
operation "add" a b = a + b
operation "mul" a b = a * b
operation "ban" a b = a .&. b
operation "bor" a b = a .|. b
operation "set" a b = a
operation "gti" a b = if a > b then 1 else 0
operation "gtr" a b = if a > b then 1 else 0
operation "eqi" a b = if a == b then 1 else 0
operation "eqr" a b = if a == b then 1 else 0

part1 :: [(V.Vector Int, (Int, Int, Int, Int), V.Vector Int)] -> Int
part1 [] = 0
part1 ((input, (_, a, b, c), output) : tests) =
    if 2 < (length matchingOperations) then 1 + part1 tests else part1 tests
  where
    matchingOperations = filter
        (\opCode -> output == runOp input opCode a b c)
        allOperationStrings

part2
    :: [(V.Vector Int, (Int, Int, Int, Int), V.Vector Int)]
    -> V.Vector String
    -> V.Vector String
part2 [] opCodes = opCodes
part2 ((input, (i, a, b, c), output) : tests) opCodes = part2 tests opCodes'
  where
    matchingOperations = filter
        (\opCode -> output == runOp input opCode a b c)
        allOperationStrings
    opCodes' = updateKnownOpCodes i matchingOperations opCodes

updateKnownOpCodes :: Int -> [String] -> V.Vector String -> V.Vector String
updateKnownOpCodes i []  opCodes = opCodes
updateKnownOpCodes i [x] opCodes = (opCodes V.// [(i, x)])
updateKnownOpCodes i xs opCodes = 
    if opCodes V.! i == "" then
        let xs' = filter (not.(`V.elem` opCodes)) xs
        in if xs' == xs then opCodes else updateKnownOpCodes i xs' opCodes
    else
        opCodes

runProgram :: V.Vector Int -> [(String, Int,Int,Int)] -> V.Vector Int
runProgram registers [] = registers
runProgram registers ((op,a,b,c):operations) = runProgram (runOp registers op a b c) operations

-------------------------------------------------------------------------
----READ INPUT-----------------------------------------------------------
-------------------------------------------------------------------------
readInput :: [String] -> [(V.Vector Int, (Int, Int, Int, Int), V.Vector Int)]
readInput []           = []
readInput ("" : lines) = readInput lines
readInput lines        = (readChunk chunk) : readInput lines'
    where (chunk, lines') = splitAt 3 lines


readChunk :: [String] -> (V.Vector Int, (Int, Int, Int, Int), V.Vector Int)
readChunk [input, operation, output] =
    ( V.fromList (read input :: [Int])
    , listTo4Tuple (map (\x -> read x :: Int) (words operation))
    , V.fromList (read output :: [Int])
    )

listTo4Tuple :: [a] -> (a, a, a, a)
listTo4Tuple [a, b, c, d] = (a, b, c, d)

readProgram :: V.Vector String -> [String] -> [(String, Int,Int,Int)]
readProgram opCodes [] = []
readProgram opCodes (line:lines) = (opCodes V.! op, a,b,c):(readProgram opCodes lines)
    where
        [op,a,b,c] = (map (\x -> read x :: Int) (words line))


-------------------------------------------------------------------------
----SOME BASIC TESTS-----------------------------------------------------
-------------------------------------------------------------------------

test :: IO ()
test = do
    print
        (runOp (V.fromList [1, 2, 3, 4]) "addr" 1 2 3 == V.fromList [1, 2, 3, 5]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "addi" 1 2 3 == V.fromList [1, 2, 3, 4]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "mulr" 1 2 3 == V.fromList [1, 2, 3, 6]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "muli" 1 2 3 == V.fromList [1, 2, 3, 4]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "banr" 1 2 3 == V.fromList [1, 2, 3, 2]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "bani" 1 5 3 == V.fromList [1, 2, 3, 0]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "borr" 1 2 3 == V.fromList [1, 2, 3, 3]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "bori" 1 2 3 == V.fromList [1, 2, 3, 2]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "setr" 0 2 3 == V.fromList [1, 2, 3, 1]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "seti" 5 2 3 == V.fromList [1, 2, 3, 5]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "gtir" 1 2 3 == V.fromList [1, 2, 3, 0]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "gtri" 1 2 3 == V.fromList [1, 2, 3, 0]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "gtrr" 2 1 3 == V.fromList [1, 2, 3, 1]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "eqir" 3 2 3 == V.fromList [1, 2, 3, 1]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "eqri" 1 2 3 == V.fromList [1, 2, 3, 1]
        )
    print
        (runOp (V.fromList [1, 2, 3, 4]) "eqrr" 1 2 3 == V.fromList [1, 2, 3, 0]
        )
