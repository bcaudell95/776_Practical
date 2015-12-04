{-# LANGUAGE GADTs, KindSignatures, DataKinds #-}

import Data.Char
import Data.List
import Data.Maybe

--------------------------------------------------
-- MAIN 'RUN' FUNCTION
--------------------------------------------------
main :: IO ()
main = do {
    run sampleEasyBoard ;
    return ()
}

run :: Board -> IO Board
run b = do {
    board <- return b ;
    printBoard board ;
    print $ countConfirmedCells board ;
	print $ boardIsValid board ;
    print "----------------------------------" ;
    newBoard <- alg1 board ;
    newBoard <- alg2 newBoard ;
    if not (newBoard == board) then (run newBoard) else return newBoard ;
}

alg1 :: Board -> IO Board
alg1 b = do {
    board <- return (map9Tuple removePossibilitiesOfCellsInGroup b) ; -- Rows
    board <- return $ columnsToRows $ (map9Tuple removePossibilitiesOfCellsInGroup (getAllColumns board)) ; -- Columns
    board <- return $ groupsToRows $ (map9Tuple removePossibilitiesOfCellsInGroup (getAllGroups board)) ; -- Groups
    return (setAllCellsWithOnlyOnePossibility board) ;
}

alg2 :: Board -> IO Board
alg2 b = do {
    board <- return $ map9Tuple setAllLoneCells b ; -- Rows
    board <- return $ columnsToRows $ map9Tuple setAllLoneCells $ getAllColumns board ; -- Columns
    board <- return $ groupsToRows $ map9Tuple setAllLoneCells $ getAllGroups board ; -- Groups
    return board
}

---------------------------------------------------
-- BASIC BOARD STRUCTURE AND UTILITY FUNCTIONS
---------------------------------------------------

data Cell :: * where
    Confirmed :: Int -> Cell
    Possibilities :: [Int] -> Cell
    Invalid :: Cell
    deriving (Show, Eq, Ord)
    
type Group = (Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell)
type Row = Group
type Column = Group
type Board = (Row, Row, Row, Row, Row, Row, Row, Row, Row)

map9Tuple :: (a -> b) -> (a, a, a, a, a, a, a, a, a) -> (b, b, b, b, b, b, b, b, b)
map9Tuple fn (a, b, c, d, e, f, g, h, i) = (fn a, fn b, fn c, fn d, fn e, fn f, fn g, fn h, fn i)

intToCell :: Int -> Cell
intToCell i
    | (0<i && i<10) = Confirmed i
    | (0==i)        = Possibilities [1..9]
    | otherwise     = Invalid

-- Type withheld because it's really long
buildBoardFromInt = map9Tuple (\ row -> map9Tuple intToCell row)

sampleFinishedBoard :: Board
sampleFinishedBoard = buildBoardFromInt ((1, 7, 2, 5, 4, 9, 6, 8, 3),(6, 4, 5, 8, 7, 3, 2, 1, 9),(3, 8, 9, 2, 6, 1, 7, 4, 5),(4, 9, 6, 3, 2, 7, 8, 5, 1),(8, 1, 3, 4, 5, 6, 9, 7, 2),(2, 5, 7, 1, 9, 8, 4, 3, 6),(9, 6, 4, 7, 1, 5, 3, 2, 8),(7, 3, 1, 6, 8, 2, 5, 9, 4),(5, 2, 8, 9, 3, 4, 1, 6, 7))

sampleEasyBoard :: Board
sampleEasyBoard = buildBoardFromInt (( 3, 1, 0, 0, 4, 0, 6, 0, 9),( 0, 8, 0, 2, 0, 0, 7, 1, 5),( 0, 0, 6, 1, 0, 0, 0, 0, 8),( 0, 0, 0, 0, 3, 4, 2, 7, 0),( 0, 0, 0, 5, 0, 6, 0, 0, 0),( 0, 4, 3, 7, 2, 0, 0, 0, 0),( 5, 0, 0, 0, 0, 2, 9, 0, 0),( 4, 7, 8, 0, 0, 9, 0, 6, 0),( 6, 0, 2, 0, 8, 0, 0, 5, 7))

sampleMediumBoard :: Board
sampleMediumBoard = buildBoardFromInt ((1, 5, 7, 9, 6, 0, 0, 0, 0),(0, 0, 8, 0, 0, 0, 9, 0, 0),(0, 0, 0, 2, 0, 0, 3, 1, 0),(0, 0, 0, 3, 1, 0, 0, 9, 0),(0, 6, 0, 8, 9, 4, 0, 5, 0),(0, 9, 0, 0, 2, 7, 0, 0, 0),(0, 2, 6, 0, 0, 1, 0, 0, 0),(0, 0, 5, 0, 0, 0, 2, 0, 0),(0, 0, 0, 0, 8, 2, 5, 6, 3))

sampleHardBoard :: Board
sampleHardBoard = buildBoardFromInt ((0, 0, 3, 0, 9, 0, 0, 2, 0),(0, 5, 0, 0, 0, 6, 0, 0, 0),(0, 2, 1, 8, 5, 0, 0, 0, 6),(0, 0, 9, 0, 0, 0, 0, 3, 5),(0, 0, 0, 0, 4, 0, 0, 0, 0),(3, 7, 0, 0, 0, 0, 1, 0, 0),(5, 0, 0, 0, 7, 4, 2, 9, 0),(0, 0, 0, 2, 0, 0, 0, 6, 0),(0, 4, 0, 0, 1, 0, 3, 0, 0))


getIth :: Int -> (a, a, a, a, a, a, a, a, a) -> a
getIth 0 (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1
getIth i (a1, a2, a3, a4, a5, a6, a7, a8, a9) 
    | (i<9) = getIth (i-1) (a2, a3, a4, a5, a6, a7, a8, a9, a1) -- Note: last element won't matter here
    | otherwise = error "invalid tuple index"

getRow :: Int -> Board -> Row
getRow i board = getIth i board

getColumn :: Int -> Board -> Column
getColumn i board = map9Tuple (\ row -> getIth i row) board

getAllColumns :: Board -> (Column, Column,Column,Column,Column,Column,Column,Column,Column)
getAllColumns b = map9Tuple (\ i -> getColumn i b) (0,1,2,3,4,5,6,7,8)

columnsToRows = getAllColumns -- Works because getAllColumns is really a transpose

listToGroup :: [Cell] -> Group
listToGroup [a1, a2, a3, a4, a5, a6, a7, a8, a9] = (a1, a2, a3, a4, a5, a6, a7, a8, a9)

getGroup :: Int -> Board -> Group
getGroup i board = listToGroup $ foldr (++) [] $ map (\ rowIndex -> map (\ colIndex -> getIth colIndex (getRow rowIndex board)) (colsForGroup i)) (rowsForGroup i)

getAllGroups :: Board -> (Group, Group, Group, Group, Group, Group, Group, Group, Group)
getAllGroups b =  map9Tuple (\ i -> getGroup i b) (0,1,2,3,4,5,6,7,8)

groupsToRows = getAllGroups -- Also works because getAllGroups is its own inverse

rowsForGroup :: Int -> [Int]
rowsForGroup i
    | (i==0 || i==1 || i==2) = [0, 1, 2]
    | (i==3 || i==4 || i==5) = [3, 4, 5]
    | (i==6 || i==7 || i==8) = [6, 7, 8]
    | otherwise = []
    
colsForGroup :: Int -> [Int]
colsForGroup i
    | (i `mod` 3 == 0) = [0, 1, 2]
    | (i `mod` 3 == 1) = [3, 4, 5]
    | (i `mod` 3 == 2) = [6, 7, 8]
    | otherwise = []
    
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

groupToList :: Group -> [Cell]
groupToList (a1, a2, a3, a4, a5, a6, a7, a8, a9) = [a1, a2, a3, a4, a5, a6, a7, a8, a9]

countConfirmedCells :: Board -> Int
countConfirmedCells b = count9Tuple $ map9Tuple countConfirmedCellsInGroup b

count9Tuple :: Num a => (a, a, a, a, a, a, a, a, a) -> a
count9Tuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = sum [a1, a2, a3, a4, a5, a6, a7, a8, a9]

countConfirmedCellsInGroup :: Group -> Int
countConfirmedCellsInGroup g = sum $ map isConfirmed $ groupToList g

isConfirmed :: Cell -> Int
isConfirmed (Confirmed _) = 1
isConfirmed _ = 0

setAllCellsWithOnlyOnePossibility :: Board -> Board
setAllCellsWithOnlyOnePossibility board = map9Tuple (\ row -> map9Tuple setValue row) board

setValue :: Cell -> Cell
setValue (Possibilities [v]) = Confirmed v
setValue c = c

boardIsValid :: Board -> Bool
boardIsValid b = not $ boardIsInvalid b

boardIsInvalid :: Board -> Bool
boardIsInvalid b = or $ [boardContainsInvalidCells b, boardContainsRowsWithMultipleOfAValue b, boardContainsRowsWithMultipleOfAValue (getAllColumns b), boardContainsRowsWithMultipleOfAValue (getAllGroups b)]

boardContainsInvalidCells :: Board -> Bool
boardContainsInvalidCells board = or9Tuple $ map9Tuple containsInvalidCells board

containsInvalidCells :: Group -> Bool
containsInvalidCells g = or9Tuple $ map9Tuple cellIsInvalid g

cellIsInvalid :: Cell -> Bool
cellIsInvalid Invalid = True
cellIsInvalid _ = False

or9Tuple :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) -> Bool
or9Tuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = or [a1, a2, a3, a4, a5, a6, a7, a8, a9]

boardContainsRowsWithMultipleOfAValue :: Board -> Bool
boardContainsRowsWithMultipleOfAValue b = or $ map (boardContainsInvalidGroupsWithValue b) [1..9]

boardContainsInvalidGroupsWithValue :: Board -> Int -> Bool
boardContainsInvalidGroupsWithValue b v = or9Tuple $ map9Tuple ((flip groupContainsMultipleOfValue) v) b

groupContainsMultipleOfValue :: Group -> Int -> Bool
groupContainsMultipleOfValue g v = (countConfirmedCellsOfValue g v) > 1

countConfirmedCellsOfValue :: Group -> Int -> Int
countConfirmedCellsOfValue g v = sum $ map ((flip isCellConfirmedOfValue) v) $ groupToList g

isCellConfirmedOfValue :: Cell -> Int -> Int
isCellConfirmedOfValue (Confirmed v) i 
	| (v == i) = 1
isCellConfirmedOfValue _ _ = 0
----------------------------------------------
-- ALGORITHM 1 - REMOVE POSSIBILITIES IF CELLS IN THE SAME ROW/COLUMN/GROUP ARE CONFIRMED TO THAT VALUE
----------------------------------------------
removePossibilitiesOfCellsInGroup :: Group -> Group
removePossibilitiesOfCellsInGroup group = map9Tuple (\ cell -> removeListFromPossibilities (gatherValuesFromFixedCells group) cell) group

gatherValuesFromFixedCells :: Group -> [Int]
gatherValuesFromFixedCells group = map (\ (Just a) -> a) $ filter isJust $ map getValueOfFixedCell $ groupToList group

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

setAllLoneCells :: Group -> Group
setAllLoneCells g = setLoneCells [1..9] g

setLoneCells :: [Int] -> Group -> Group
setLoneCells [] g = g
setLoneCells (x:xs) g = setLoneCells (xs) (setLoneCellInGroup g x)

setLoneCellInGroup :: Group -> Int -> Group
setLoneCellInGroup g v
    | ((listOfCellsThatCanTakeValue g v) == 1) = setAnyCellThatCanAcceptValue g v
    | otherwise = g

setAnyCellThatCanAcceptValue :: Group -> Int -> Group
setAnyCellThatCanAcceptValue g v = map9Tuple ((flip setValueIfPossible) v) g

setValueIfPossible :: Cell -> Int -> Cell
setValueIfPossible (Possibilities list) v
    | (v `elem` list && (length list)==1 ) = (Confirmed v)
    | otherwise = (Possibilities list)
setValueIfPossible c _ = c

listOfCellsThatCanTakeValue :: Group -> Int -> Int
listOfCellsThatCanTakeValue g v = length $ filter ((flip canCellTakeValue) v) (groupToList g)

canCellTakeValue :: Cell -> Int -> Bool
canCellTakeValue (Possibilities list) v = v `elem` list
canCellTakeValue _ _ = False
