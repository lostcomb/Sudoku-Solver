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

isFull :: Entry -> Bool
isFull (Full _) = True
isFull _        = False

isEmpty :: Entry -> Bool
isEmpty Empty = True
isEmpty _     = False

-- The data type for the Sudoku board. This data type contains
-- the size of the boxes, and the rows of the board.
data Board = Board Int [[Entry]]
  deriving (Eq)

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

chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs <= n = [xs]
  | otherwise      = take n xs : chunk n (drop n xs)

getRows :: Board -> [[Entry]]
getRows (Board _ rows) = rows

getColumns :: Board -> [[Entry]]
getColumns (Board _ rows) = transpose rows

getBoxes :: Board -> [[Entry]]
getBoxes (Board _ rows) = undefined --TODO

-- Need a function that returns all possible values for a particular location.
-- This should take into account values in current box, values in current row
-- and values in current column. May want to re-think using lists here.

updateEntry :: Board -> Int -> Int -> Entry -> Board
updateEntry (Board _ rows) row col entry = undefined --TODO

complete :: Board -> Bool
complete (Board _ rows)
  = foldr (\line acc -> (foldr (\entry acc -> acc && isFull entry)
                               True
                               line) && acc) True rows
