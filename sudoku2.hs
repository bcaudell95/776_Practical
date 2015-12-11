{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

import Data.Char
import Data.List hiding (transpose)
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

---------------------------------------------------
-- MAIN
---------------------------------------------------

run :: Board -> Board
run b = (\ (Just a) -> a) $ solve $ performAlg2 $ performAlg1 b

solve :: Board -> Maybe Board
solve b = if boardIsInvalid b then Nothing else
    if boardIsFinished b then Just b else 
    (\ ms -> if length ms >0 then head ms else Nothing) $
    filter isJust $
    map (solve . performAlg2 . performAlg1) $ 
    (\ (r, c, ps) ->  
        map (\ p -> setCellInBoard (r,c) p b) ps
    ) $ 
    findCellWithFewestPossibilities b

setCellInBoard :: (Int, Int) -> Int -> Board -> Board
setCellInBoard pos val b =
    joinBoards b (boardWithOneCell pos val)  

blankCell :: Cell
blankCell = Possibilities [1..9]

emptyUnit :: Unit
emptyUnit = listToUnit $ replicate 9 blankCell

oneCellUnit :: Int -> Int -> Unit
oneCellUnit pos val 
    = listToUnit $ (replicate pos blankCell) ++ [(Confirmed val)] ++ (replicate (9-pos-1) blankCell)

boardWithOneCell :: (Int, Int) -> Int -> Board
boardWithOneCell (row, col) val
    = listToBoard $
    (replicate row emptyUnit) ++ [oneCellUnit col val] ++ (replicate (9-row-1) emptyUnit)

------------------------------------------------------------------
-- BASIC DATA TYPES AND UTILITY FUNCTIONS
------------------------------------------------------------------

data Cell :: * where
    Confirmed :: Int -> Cell
    Possibilities :: [Int] -> Cell
    Invalid :: Cell
    deriving (Show)

type Unit = (Cell,Cell,Cell,Cell,Cell,Cell,Cell,Cell,Cell)
type Row = Unit
type Column = Unit
type Box = Unit

type Board = (Row,Row,Row,Row,Row,Row,Row,Row,Row)  

map9Tuple :: (a -> b) -> (a, a, a, a, a, a, a, a, a) -> (b, b, b, b, b, b, b, b, b)
map9Tuple fn (a, b, c, d, e, f, g, h, i) = (fn a, fn b, fn c, fn d, fn e, fn f, fn g, fn h, fn i)

intToCell :: Int -> Cell
intToCell i
    | (0<i && i<10) = Confirmed i
    | (0==i)        = Possibilities [1..9]
    | otherwise     = Invalid

-- Type withheld because it's really long
buildBoardFromInt = map9Tuple (\ row -> map9Tuple intToCell row)

blankBoard :: Board
blankBoard = listToBoard $ map listToUnit $ replicate 9 $ replicate 9 (Possibilities [1..9])

sampleFinishedBoard :: Board
sampleFinishedBoard = buildBoardFromInt ((1, 7, 2, 5, 4, 9, 6, 8, 3),(6, 4, 5, 8, 7, 3, 2, 1, 9),(3, 8, 9, 2, 6, 1, 7, 4, 5),(4, 9, 6, 3, 2, 7, 8, 5, 1),(8, 1, 3, 4, 5, 6, 9, 7, 2),(2, 5, 7, 1, 9, 8, 4, 3, 6),(9, 6, 4, 7, 1, 5, 3, 2, 8),(7, 3, 1, 6, 8, 2, 5, 9, 4),(5, 2, 8, 9, 3, 4, 1, 6, 7))

sampleEasyBoard :: Board
sampleEasyBoard = buildBoardFromInt (( 3, 1, 0, 0, 4, 0, 6, 0, 9),( 0, 8, 0, 2, 0, 0, 7, 1, 5),( 0, 0, 6, 1, 0, 0, 0, 0, 8),( 0, 0, 0, 0, 3, 4, 2, 7, 0),( 0, 0, 0, 5, 0, 6, 0, 0, 0),( 0, 4, 3, 7, 2, 0, 0, 0, 0),( 5, 0, 0, 0, 0, 2, 9, 0, 0),( 4, 7, 8, 0, 0, 9, 0, 6, 0),( 6, 0, 2, 0, 8, 0, 0, 5, 7))

sampleMediumBoard :: Board
sampleMediumBoard = buildBoardFromInt ((1, 5, 7, 9, 6, 0, 0, 0, 0),(0, 0, 8, 0, 0, 0, 9, 0, 0),(0, 0, 0, 2, 0, 0, 3, 1, 0),(0, 0, 0, 3, 1, 0, 0, 9, 0),(0, 6, 0, 8, 9, 4, 0, 5, 0),(0, 9, 0, 0, 2, 7, 0, 0, 0),(0, 2, 6, 0, 0, 1, 0, 0, 0),(0, 0, 5, 0, 0, 0, 2, 0, 0),(0, 0, 0, 0, 8, 2, 5, 6, 3))

sampleHardBoard :: Board
sampleHardBoard = buildBoardFromInt ((0, 0, 3, 0, 9, 0, 0, 2, 0),(0, 5, 0, 0, 0, 6, 0, 0, 0),(0, 2, 1, 8, 5, 0, 0, 0, 6),(0, 0, 9, 0, 0, 0, 0, 3, 5),(0, 0, 0, 0, 4, 0, 0, 0, 0),(3, 7, 0, 0, 0, 0, 1, 0, 0),(5, 0, 0, 0, 7, 4, 2, 9, 0),(0, 0, 0, 2, 0, 0, 0, 6, 0),(0, 4, 0, 0, 1, 0, 3, 0, 0))

sampleExtremeBoard :: Board
sampleExtremeBoard = buildBoardFromInt ((8, 0, 0, 0, 0, 0, 0, 0, 0),(0, 0, 3, 6, 0, 0, 0, 0, 0),(0, 7, 0, 0, 9, 0, 2, 0, 0),(0, 5, 0, 0, 0, 7, 0, 0, 0),(0, 0, 0, 0, 4, 5, 7, 0, 0),(0, 0, 0, 1, 0, 0, 0, 3, 0),(0, 0, 1, 0, 0, 0, 0, 6, 8),(0, 0, 8, 5, 0, 0, 0, 1, 0),(0, 9, 0, 0, 0, 0, 4, 0, 0))

getIth :: Int -> (a, a, a, a, a, a, a, a, a) -> a
getIth 0 (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1
getIth i (a1, a2, a3, a4, a5, a6, a7, a8, a9) 
    | (i<9) = getIth (i-1) (a2, a3, a4, a5, a6, a7, a8, a9, a1) -- Note: last element won't matter here
    | otherwise = error "invalid tuple index"

cellToChar :: Cell -> Char
cellToChar (Confirmed c)        = intToDigit c
cellToChar (Possibilities _)    = '_'
cellToChar Invalid              = 'X'
    
printRow :: Row -> IO ()
printRow (a1, a2, a3, a4, a5, a6, a7, a8, a9) = do {
    print $ intersperse ' ' $ (map cellToChar [a1, a2, a3, a4, a5, a6, a7, a8, a9])
}

printBoard :: Board -> IO ()
printBoard (r1, r2, r3, r4, r5, r6, r7, r8, r9) = do {
    sequence $ map printRow [r1, r2, r3, r4, r5, r6, r7, r8, r9] ;
    return ()
} 

listToUnit :: [Cell] -> Unit
listToUnit [a1, a2, a3, a4, a5, a6, a7, a8, a9] = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

unitToList :: Unit -> [Cell]
unitToList (a1, a2, a3, a4, a5, a6, a7, a8, a9) = [a1, a2, a3, a4, a5, a6, a7, a8, a9]

andOver9Tuple :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Bool
andOver9Tuple (a1,a2,a3,a4,a5,a6,a7,a8,a9) = and [a1,a2,a3,a4,a5,a6,a7,a8,a9] 
listToBoard :: [Unit] -> Board
listToBoard [a1, a2, a3, a4, a5, a6, a7, a8, a9] = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

boardToList :: Board -> [Unit]
boardToList (a1, a2, a3, a4, a5, a6, a7, a8, a9) = [a1, a2, a3, a4, a5, a6, a7, a8, a9]

-----------------------------------------------------------
-- BOARD MODIFICATION AND READING FUNCTIONS
-----------------------------------------------------------

getCell :: (Int, Int) -> Board -> Cell
getCell (row, col) b = getIth col $ getIth row b

getRow :: Int -> Board -> Row
getRow i b = getIth i b

getCol :: Int -> Board -> Column
getCol i b = map9Tuple (\ row -> getIth i row) b

transpose :: Board -> Board -- Rows <-> Columns
transpose b = map9Tuple (\ i -> getCol i b) (0,1,2,3,4,5,6,7,8)


rowsForBox :: Int -> [Int]
rowsForBox i
    | 0<=i && i<3 = [0,1,2]
    | 3<=i && i<6 = [3,4,5]
    | 6<=i && i<9 = [6,7,8]
    | otherwise = []

colsForBox :: Int -> [Int]
colsForBox i
    | i `mod` 3 == 0 = [0,1,2]
    | i `mod` 3 == 1 = [3,4,5]
    | i `mod` 3 == 2 = [6,7,8]
    | otherwise = []

getBox :: Int -> Board -> Box
getBox i b = listToUnit $ [getCell (row,col) b | row<-(rowsForBox i), col<-(colsForBox i)] 

swapRowsAndBoxes :: Board -> Board -- is its own inverse
swapRowsAndBoxes b = map9Tuple (\ i -> getBox i b) (0,1,2,3,4,5,6,7,8)

-- Type withheld because it's very long
mapOverBoard b fn = map9Tuple (\ row -> map9Tuple fn row) b

cellIsConfirmed :: Cell -> Bool
cellIsConfirmed (Confirmed _) = True
cellIsConfirmed _ = False

unitIsConfirmed :: Unit -> Bool
unitIsConfirmed u = and $ map cellIsConfirmed $ unitToList u

boardIsFinished :: Board -> Bool
boardIsFinished b = andOver9Tuple $ map9Tuple unitIsConfirmed b 

boardIsValid :: Board -> Bool
boardIsValid b = not $ boardIsInvalid b

boardIsInvalid :: Board -> Bool
boardIsInvalid b = or $ [boardContainsInvalidCells b, boardContainsRowsWithMultipleOfAValue b, boardContainsRowsWithMultipleOfAValue (transpose b), boardContainsRowsWithMultipleOfAValue (swapRowsAndBoxes b)]

boardContainsInvalidCells :: Board -> Bool
boardContainsInvalidCells board = or9Tuple $ map9Tuple containsInvalidCells board

containsInvalidCells :: Unit -> Bool
containsInvalidCells g = or9Tuple $ map9Tuple cellIsInvalid g

cellIsInvalid :: Cell -> Bool
cellIsInvalid Invalid = True
cellIsInvalid (Possibilities []) = True
cellIsInvalid _ = False

or9Tuple :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Bool
or9Tuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = or [a1, a2, a3, a4, a5, a6, a7, a8, a9]

boardContainsRowsWithMultipleOfAValue :: Board -> Bool
boardContainsRowsWithMultipleOfAValue b = or $ map (boardContainsInvalidGroupsWithValue b) [1..9]

boardContainsInvalidGroupsWithValue :: Board -> Int -> Bool
boardContainsInvalidGroupsWithValue b v = or9Tuple $ map9Tuple ((flip groupContainsMultipleOfValue) v) b

groupContainsMultipleOfValue :: Unit -> Int -> Bool
groupContainsMultipleOfValue g v = (countConfirmedCellsOfValue g v) > 1

countConfirmedCellsOfValue :: Unit -> Int -> Int
countConfirmedCellsOfValue g v = sum $ map ((flip isCellConfirmedOfValue) v) $ unitToList g

isCellConfirmedOfValue :: Cell -> Int -> Int
isCellConfirmedOfValue (Confirmed v) i 
    | (v == i) = 1
isCellConfirmedOfValue _ _ = 0

joinCells :: Cell -> Cell -> Cell
joinCells (Confirmed c) _ = Confirmed c
joinCells _ (Confirmed c) = Confirmed c
joinCells (Possibilities ps) (Possibilities rs) = Possibilities (intersect ps rs)
joinCells _ _ = Invalid

joinUnits :: Unit -> Unit -> Unit
joinUnits x y = listToUnit $ zipWith joinCells (unitToList x) (unitToList y)

joinBoards :: Board -> Board -> Board
joinBoards b1 b2 = listToBoard $ zipWith joinUnits (boardToList b1) (boardToList b2)


----------------------------------------------------------------
-- SEARCH UTILITY FUNCTIONS
----------------------------------------------------------------
getPossibilities :: Cell -> [Int]
getPossibilities (Confirmed _) = []
getPossibilities (Possibilities ps) = ps
getPossibilities (Invalid) = []

listPossibilities :: Board -> [(Int, Int, [Int])]
listPossibilities b = [(row, col, getPossibilities $ getCell (row,col) b) | row<-[0..8], col<-[0..8]]

filterDownPossibilities :: [(Int, Int, [Int])] -> [(Int, Int, [Int])]
filterDownPossibilities xs = filter (\ (_, _, ps) -> length ps > 0) xs

takeTripleOfFewerPossiblities :: (Int, Int, [Int]) -> (Int, Int, [Int]) -> (Int, Int, [Int])
takeTripleOfFewerPossiblities (x1, y1, ps) (x2, y2, rs) 
    = if (length ps)<(length rs) then (x1, y1, ps) else (x2, y2, rs)

findCellWithFewestPossibilities :: Board -> (Int, Int, [Int])
findCellWithFewestPossibilities b = foldr takeTripleOfFewerPossiblities (-1,-1,[1..10]) $ filterDownPossibilities $ listPossibilities b

----------------------------------------------
-- ALGORITHM 1 - REMOVE POSSIBILITIES IF CELLS IN THE SAME ROW/COLUMN/GROUP ARE CONFIRMED TO THAT VALUE
----------------------------------------------
performAlg1 :: Board -> Board
performAlg1 b = foldr joinBoards blankBoard 
    [map9Tuple removePossibilitiesOfCellsInUnit b,
     transpose $ map9Tuple removePossibilitiesOfCellsInUnit $ transpose b,
     swapRowsAndBoxes $ map9Tuple removePossibilitiesOfCellsInUnit $ swapRowsAndBoxes b] 

removePossibilitiesOfCellsInUnit :: Unit -> Unit
removePossibilitiesOfCellsInUnit group = map9Tuple (\ cell -> removeListFromPossibilities (gatherValuesFromFixedCells group) cell) group

gatherValuesFromFixedCells :: Unit -> [Int]
gatherValuesFromFixedCells group = map (\ (Just a) -> a) $ filter isJust $ map getValueOfFixedCell $ unitToList group

getValueOfFixedCell :: Cell -> Maybe Int
getValueOfFixedCell (Confirmed v) = Just v
getValueOfFixedCell _ = Nothing

removeFromPossibilities :: Int -> Cell -> Cell
removeFromPossibilities i (Possibilities ps)
    | (i `elem` ps) = Possibilities (delete i ps)
    | otherwise = Possibilities ps
removeFromPossibilities _ c = c

removeListFromPossibilities :: [Int] -> Cell -> Cell
removeListFromPossibilities [] c = c
removeListFromPossibilities (x:xs) c = removeListFromPossibilities xs (removeFromPossibilities x c)

--------------------------------------------------------
-- ALGORITHM 2 - SEE IF A GROUP ONLY HAS ONE CELL THAT CAN BE A PARTICULAR VALUE
--------------------------------------------------------

performAlg2 :: Board -> Board
performAlg2 b = foldr joinBoards blankBoard $
    [setLoneCellsInBoard b,
     transpose $ setLoneCellsInBoard $ transpose b,
     swapRowsAndBoxes $ setLoneCellsInBoard $ swapRowsAndBoxes b]

setLoneCellsInBoard :: Board -> Board
setLoneCellsInBoard b = map9Tuple setAllLoneCells b

setAllLoneCells :: Unit -> Unit
setAllLoneCells g = setLoneCells [1..9] g

setLoneCells :: [Int] -> Unit -> Unit
setLoneCells [] g = g
setLoneCells (x:xs) g = setLoneCells (xs) (setLoneCellInUnit g x)

setLoneCellInUnit :: Unit -> Int -> Unit
setLoneCellInUnit g v
    | ((listOfCellsThatCanTakeValue g v) == 1) = setAnyCellThatCanAcceptValue g v
    | otherwise = g

setAnyCellThatCanAcceptValue :: Unit -> Int -> Unit
setAnyCellThatCanAcceptValue g v = map9Tuple ((flip setValueIfPossible) v) g

setValueIfPossible :: Cell -> Int -> Cell
setValueIfPossible (Possibilities list) v
    | (v `elem` list && (length list)==1 ) = (Confirmed v)
    | otherwise = (Possibilities list)
setValueIfPossible c _ = c

listOfCellsThatCanTakeValue :: Unit -> Int -> Int
listOfCellsThatCanTakeValue g v = length $ filter ((flip canCellTakeValue) v) (unitToList g)

canCellTakeValue :: Cell -> Int -> Bool
canCellTakeValue (Possibilities list) v = v `elem` list
canCellTakeValue _ _ = False
