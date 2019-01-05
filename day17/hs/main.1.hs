{-# LANGUAGE FlexibleContexts #-}

import           Data.Char
import Data.List
import qualified Data.HashSet                  as S
import qualified Data.Array as A
import qualified Data.Array.IArray as I
import Data.Array.MArray
import Data.Array.ST (runSTArray)

type Location = (Int,Int)
type Bounds = (Int,Int,Int,Int)
data Element = Empty | Clay | RunningWater | StandingWater deriving (Show,Eq)

main = do
    content <- readFile "test.txt"
    let clayList = readInput $ lines content
    let (minx,maxx,miny,maxy) = getClayBounds clayList
    let sceneF = A.accumArray (flip const) Empty ((minx-1,miny-1), (maxx+1,maxy+1)) (zip clayList (repeat Clay))
    showSquares sceneF
    let sceneF' = runSTArray $ do
         scene <- thaw sceneF
         fillWater maxy (500,0) scene
         return scene
    showSquares sceneF'
    --print scene
    -- let clay = S.fromList clayList
    -- let water = (S.empty,S.empty)
    -- let bounds@(_,_,miny,maxy) = getBounds clayList
    -- print bounds
    -- --showSquares bounds clay water
    -- filled <- fillWater maxy (500,0) sceneM
    -- print filled
    -- let allWater = S.union standingWater runningWater
    -- putStrLn ""
    -- showSquares bounds clay water'
    -- print $ S.toList standingWater
    -- print $ S.size allWater

fillWater :: (MArray a Element m, Integral i, Ix i, Show i) => i -> (i,i) -> a (i, i) Element -> m Bool
fillWater maxy current@(x, y) scene = do
    showSquares $ freeze scene
    let downOne      = (x, y + 1)
    canMoveDown  <- canMove downOne scene
    let leftOne      = (x - 1, y)
    canMoveLeft  <- canMove leftOne scene   
    let rightOne     = (x + 1, y)
    canMoveRight <- canMove rightOne scene
    if y >= maxy then
        return False
    else 
        if canMoveDown then do
            moved <- fillWater maxy downOne scene
            if moved then
                fillWater maxy current scene
            else
                return False
        else 
            if canMoveLeft && canMoveRight then do
                writeArray scene leftOne RunningWater
                writeArray scene rightOne RunningWater
                leftStanding <- fillWater maxy leftOne scene
                rightStanding <- fillWater maxy rightOne scene                    
                if leftStanding && rightStanding then
                    return True
                else do
                    neighbours <- getNeighbours current scene
                    False <$ sequence_ [writeArray scene p RunningWater | p <- neighbours]
            else 
                if canMoveLeft then moveLeftOrRight maxy current leftOne scene
                else 
                    if canMoveRight then moveLeftOrRight maxy current rightOne scene
                    else do
                        writeArray scene current StandingWater
                        return True    

moveLeftOrRight maxy current next scene = do
    moved <- fillWater maxy next scene
    if moved then do
        writeArray scene current StandingWater
        return True
    else
        return False

getNeighbours ::(MArray a Element m, Integral i, Ix i, Show i) => (i,i) -> a (i,i) Element -> m [(i,i)]
getNeighbours p@(x,y) scene = do
    v <- readArray scene p
    if v /= Clay then do
        left <- getLeftNeighbours (x-1,y) scene
        right <- getRightNeighbours (x+1,y) scene
        return (left ++ [p] ++ right)
    else 
        return []

getRightNeighbours ::(MArray a Element m, Integral i, Ix i, Show i) => (i,i) -> a (i,i) Element -> m [(i,i)]
getRightNeighbours p@(x,y) scene = do
    v <- readArray scene p
    if v /= Clay then do
        r <- getRightNeighbours (x+1,y) scene
        return (p:r)
    else
        return []


getLeftNeighbours ::(MArray a Element m, Integral i, Ix i, Show i) => (i,i) -> a (i,i) Element -> m [(i,i)]
getLeftNeighbours p@(x,y) scene = do
    v <- readArray scene p
    if v /= Clay then do 
        l <- getLeftNeighbours (x-1,y) scene
        return (p:l)
    else
        return []


canMove :: (MArray a Element m, Integral i, Ix i, Show i) => (i, i) -> a (i,i) Element -> m Bool
canMove location scene = do
    val <- readArray scene location 
    return (val == Empty)

readInput :: [String] -> [(Int, Int)]
readInput []       = []
readInput (l : ls) = [ (x, y) | y <- [y1 .. y2], x <- [x1 .. x2] ]
    ++ readInput ls
  where
    [single, range] = words l
    (x1    , x2   ) = if head single == 'x'
        then let i = read (filter isDigit single) :: Int in (i, i)
        else splitRange range
    (y1, y2) = if head single == 'y'
        then let i = read (filter isDigit single) :: Int in (i, i)
        else splitRange range

--y=1077..1087
splitRange :: String -> (Int, Int)
splitRange range = (start, end)
  where
    range'             = drop 2 range
    (startStr, endStr) = span isDigit range'
    start              = read startStr :: Int
    end                = read (drop 2 endStr) :: Int

showSquares squares = do
    let ((minx,miny),(maxx,maxy)) = I.bounds squares
    putStrLn $ foldl1 (\agg a -> agg ++ "\n" ++ a) [[ showCell (x,y) squares | x<-[minx..maxx]]| y<-[miny..maxy]]

showCell p squares = case squares A.! p of
    Empty -> '.'
    Clay -> '#'
    RunningWater -> '|'
    StandingWater -> '~'

-- showSquaresM squares = do
--     ((minx,miny),(maxx,maxy)) <- getBounds squares
--     putStrLn $ foldl1 (\agg a -> do
--         a' <- a
--         agg ++ "\n" ++ [a']) [[ showCellM (x,y) squares | x<-[minx..maxx]]| y<-[miny..maxy]]

-- showCellM p squares = do
--     v <- readArray squares p
--     case v of
--         Empty -> return '.'
--         Clay -> return '#'
--         RunningWater -> return '|'
--         StandingWater -> return '~'
    
            
getClayBounds :: [(Int,Int)] -> (Int,Int,Int,Int)
getClayBounds clay = 
    let minx = minimum $ map fst clay
        maxx = maximum $ map fst clay
        miny = minimum $ map snd clay
        maxy = maximum $ map snd clay
    in (minx,maxx,miny,maxy)