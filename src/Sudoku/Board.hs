module Sudoku.Board where

import Sudoku.Entry

import Data.List
import Data.Matrix

-- The data type for constraints that valid solutions must
-- satisfy.
data Constraint
  = Unique     [(Int, Int)]
  | Sum    Int [(Int, Int)]
  | Column Int
  | Row    Int
  | Box    (Int, Int, Int, Int)
  deriving (Show, Eq, Read)

-- The data type for the Sudoku board.
data Board
  = Board { noFull  :: Int
          , boxSize :: Int
          , mat     :: Matrix Entry
          , cs      :: [Constraint]
          } deriving (Eq)

-- Pretty print the Sudoku board.
instance Show Board where
  show board = concat
             . intersperse "\n"
             . concat
             . intersperse [lineStr]
             . chunk n
             . map showRow
             $ toLists (mat board)
    where n        = boxSize board
          maxESize = length (show (n^2 - 1))
          boxWidth = maxESize * n + n
          middle   = replicate (boxWidth + 1) '-'
          end      = replicate boxWidth '-'
          lineStr  = concat
                   . intersperse "+"
                   $ end : replicate (n - 2) middle ++ [end]
          showRow :: [Entry] -> String
          showRow row = concat . intersperse " | " $ chunkStrs
            where chunks    = chunk n row
                  chunkStrs = map (concat . intersperse " " . map showEntry)
                                  chunks
          showEntry :: Entry -> String
          showEntry entry = replicate padNo ' ' ++ entryStr
            where entryStr = show entry
                  padNo    = maxESize - length entryStr

-- This function splits the specified list into sublists of length
-- n. The last sublist may be of length < n if n does not divide the
-- length of the specified list.
chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs <= n = [xs]
  | otherwise      = c : chunk n xs'
  where (c, xs') = splitAt n xs

-- This function adds the specified constraints to the specified
-- board.
addConstraints :: [Constraint] -> Board -> Board
addConstraints cs' board = board { cs = cs board ++ cs' }

-- This function updates the entry at the specified (1-index) position
-- in the board.
updateEntry :: Board -> Int -> Int -> Entry -> Board
updateEntry board row col entry
  = board { mat    = setElem entry (row, col) (mat board)
          , noFull = noFull'
          }
  where noFull'     = if outOfBounds then noFull board
                                     else noFull board + 1
        boardSize   = (boxSize board) ^ 2
        outOfBounds =  row < 1         || col < 1
                    || row > boardSize || col > boardSize

{- Generate Boards -}

-- This function returns the Board specified by the list of rows.
fromList :: [[Entry]] -> Board
fromList board
  = Board { noFull  = length . filter isFull . concat $ board
          , boxSize = n
          , mat     = fromLists board
          , cs      = defaultConstraints n
          }
  where n = floor . sqrt . fromIntegral $ (length board)

-- This function returns an empty board of size n^2.
emptyBoard :: Int -> Board
emptyBoard n
  = Board { noFull  = 0
          , boxSize = n
          , mat     = matrix (n^2) (n^2) (const Empty)
          , cs      = defaultConstraints n
          }

-- This function returns the default constraints for a Sudoku board.
-- i.e. the Row, Column and Box constraints. n is the size of the
-- boxes.
defaultConstraints :: Int -> [Constraint]
defaultConstraints n =  map Row [1..n^2]
                     ++ map Column [1..n^2]
                     ++ boxes
  where boxes = [ Box (rowStart, rowEnd, colStart, colEnd)
                | (rowStart, rowEnd) <- boxIndices
                , (colStart, colEnd) <- boxIndices
                ]
        boxIndices = [(n*(i - 1) + 1, n*i) | i <- [1..n]]
