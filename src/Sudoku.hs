module Sudoku where

import Data.List

{-
  This module provides the data types to represent the state of the Sudoku game.
  This module provides the functions to help check for valid board states.
-}

-- The data type for the entries in the Sudoku board.
data Entry
  = Full Int
  | Empty
  deriving (Eq)

-- Print the correct character for each constructor.
instance Show Entry where
  show (Full i) = show i
  show Empty    = "."

-- This function returns True if the specified Entry is Full.
isFull :: Entry -> Bool
isFull (Full _) = True
isFull _        = False

-- This function returns True if the specified Entry is Empty.
isEmpty :: Entry -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Type synonyms to make better use of the type system.
type Set = [Entry]
type Row = Set

-- The data type for the Sudoku board. This data type contains
-- the size of the boxes, and the rows of the board.
data Board = Board Int [Row]
  deriving (Eq)

-- This function returns the size of the specified board.
boardSize :: Board -> Int
boardSize (Board _ rows) = length rows

-- This function returns the size of the boxes of the specified
-- board.
boxSize :: Board -> Int
boxSize (Board n _) = n

-- Pretty print the Sudoku board.
instance Show Board where
  show (Board n rows) = unlines
                      . concat
                      . map (\box -> [lineStr] ++ box ++ [lineStr])
                      . map (intersperse lineStr)
                      . chunk n
                      . map rowStr $ rows
    where p           = length . show $ (n * n)
          rowStr      = concat
                      . map (\box -> "| " ++ box ++ " |")
                      . map (intercalate " | ")
                      . chunk n
                      . map (pad p . show)
          lineStr     = concat
                      . replicate (length rows `div` n) $ boxStr
          boxStr      = "+" ++ replicate (n * (p + 2) + (n - 1)) '-' ++ "+"
          pad n str   = replicate (n - length str) ' ' ++ str

-- This function returns the list of rows of the specified board.
getRows :: Board -> [Set]
getRows (Board _ rows) = rows

-- This function returns the list of columns of the specified board.
getColumns :: Board -> [Set]
getColumns (Board _ rows) = transpose rows

-- This function returns the list of boxes of the specified board.
getBoxes :: Board -> [Set]
getBoxes (Board n rows) = boxes
  where rows' = chunk n . map (chunk n) $ rows
        boxes = concat . map zipN $ rows'
        zipN :: [[[a]]] -> [[a]]
        zipN xs
          | (sum . map length) xs == 0 = []
          | otherwise                  = box : zipN xs'
          where box = concat . map head $ xs
                xs' = map tail xs

-- This function returns True if the specified set does not contain any
-- duplicates.
validSet :: Set -> Bool
validSet s = unique [] s'
  where s' = filter (/=Empty) s
        unique :: (Eq a) => [a] -> [a] -> Bool
        unique _       []   = True
        unique working (x:xs)
          | x `elem` working = False
          | otherwise        = unique (x:working) xs

-- Need a set of functions that return True if the specified row/col/box
-- is correct with respect to the board. These rows/cols/boxes can contain
-- empty entries. These functions will be used to prune the search tree.

-- Need a function that returns all possible values for a particular location.
-- This should take into account values in current box, values in current row
-- and values in current column. May want to re-think using lists here.

-- This function updates the entry at the specified (0-index) position
-- in the board. If the specified position is off the board, this function
-- makes no change to the board.
updateEntry :: Board -> Int -> Int -> Entry -> Board
updateEntry (Board n rows) row_i col_i entry
  | outOfBounds = Board n rows
  | otherwise   = Board n rows'
  where row         = rows !! row_i
        row'        = update col_i entry row
        rows'       = update row_i row' rows
        n2          = length rows
        outOfBounds =  row_i >= n2 || col_i >= n2
                    || row_i <  0  || col_i <  0

-- This function returns True if the board is complete.
-- i.e. this function returns True if all entries in the board
-- are Full.
complete :: Board -> Bool
complete (Board _ rows) = and . map (and . map isFull) $ rows

-- This function returns a uniform 9x9 board.
uniform9x9Board :: Entry -> Board
uniform9x9Board = uniformBoard 3

-- This function returns a square board of size n^2 with all entries the same.
uniformBoard :: Int -> Entry -> Board
uniformBoard n e = Board n rows
  where n2   = n * n
        row  = replicate n2 e
        rows = replicate n2 row

-- This function updates the element at the specified index of the list.
-- If the specified position is <= 0, the element is appended to the head
-- of the list. If the specified position is >= length of the list, the
-- element appended to the end of the list.
update :: Int -> a -> [a] -> [a]
update index x xs = h_xs ++ x:t_xs
  where h_xs = take index xs
        t_xs = drop (index + 1) xs

-- This function returns a list containing the contiguous chunks of size
-- n of the specified list. The last chunk can be of size less than n.
chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs <= n = [xs]
  | otherwise      = take n xs : chunk n (drop n xs)
