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

instance Show Entry where
  show (Full i) = show i
  show Empty    = "."

isFull :: Entry -> Bool
isFull (Full _) = True
isFull _        = False

-- An enum to indicate whether the board is represented as a list of rows,
-- or a list of columns.
data Rep
  = Row
  | Col
  deriving (Show, Eq, Read)

neg :: Rep -> Rep
neg Row = Col
neg Col = Row

-- The data type for the Sudoku board.
data Board = Board Int Rep [[Entry]]
  deriving (Eq)

-- Pretty print the Sudoku board.
instance Show Board where
  show (Board n Row board) = unlines
                           . concat
                           . map (\box -> [lineStr] ++ box ++ [lineStr])
                           . map (intersperse lineStr)
                           . chunk n
                           . map rowStr $ board
    where p         = length . show $ (n * n)
          rowStr    = concat
                    . map (\box -> "| " ++ box ++ " |")
                    . map (intercalate " | ")
                    . chunk n
                    . map (pad p . show)
          lineStr   = concat
                    . replicate (length board `div` n) $ boxStr
          boxStr    = "+" ++ replicate (n * (p + 2) + (n - 1)) '-' ++ "+"
          pad n str = replicate (n - length str) ' ' ++ str
  show board               = show $ transposeBoard board

chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs <= n = [xs]
  | otherwise      = take n xs : chunk n (drop n xs)

transposeBoard :: Board -> Board
transposeBoard (Board n rep board) = Board n (neg rep) (transpose board)

updateEntry :: Board -> Int -> Int -> Entry -> Board
updateEntry (Board _ rep old) x y entry = undefined

complete :: Board -> Bool
complete (Board _ _ board)
  = foldr (\line acc -> (foldr (\entry acc -> acc && isFull entry)
                               True
                               line) && acc) True board
