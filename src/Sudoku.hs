module Sudoku
( module Sudoku
, module Sudoku.Entry
, module Sudoku.Board
) where

import Sudoku.Entry
import Sudoku.Board

import Data.List
import Data.Matrix

{-
  This module provides the data types to represent the state of the Sudoku game.
  This module provides the functions to help check for valid board states.
-}

-- This function returns the possible values for the specified entry.
-- If the specified entry already has a value, said value is the only
-- value returned.
validEntries :: Board -> Int -> Int -> [Entry]
validEntries board row col
  | outOfBounds  = []
  | isFull entry = [entry]
  | otherwise    = intersectAll . map (possibleValues board) $ cs'
  where boardSize   = (boxSize board) ^ 2
        outOfBounds =  row < 1         || col < 1
                    || row > boardSize || col > boardSize
        entry       = getElem row col (mat board)
        cs'         = filter (inConstraint row col) (cs board)

-- This function returns True if the specified coordinates are
-- constrained by the specified constraint.
inConstraint :: Int -> Int -> Constraint -> Bool
inConstraint row col c = case c of
  Unique   cs          -> elem (row, col) cs
  Sum    _ cs          -> elem (row, col) cs
  Column i             -> col == i
  Row    i             -> row == i
  Box (sR, eR, sC, eC) ->  sR <= row && row <= eR
                        && sC <= col && col <= eC

-- This function returns the possible values given a constraint.
possibleValues :: Board -> Constraint -> [Entry]
possibleValues board c = case c of
  Unique cs            -> uniqueC . map getElem' $ cs
  Sum  s cs            -> sumC  s . map getElem' $ cs
  Column i             -> uniqueC . getCol i . mat $ board
  Row    i             -> uniqueC . getRow i . mat $ board
  Box (sR, eR, sC, eC) -> uniqueC . submatrix sR eR sC eC . mat $ board
  where n              = (boxSize board) ^ 2
        getElem' (i,j) = getElem i j (mat board)
        uniqueC set    = filter ((flip notElem) set) [Full 1..Full n]
        sumC s  set    = undefined -- Solve using dynamic programming.
          where noRemaining = length . filter (==Empty) $ set

-- This function returns the coordinates of all empty entries in the
-- Sudoku board.
emptyEntries :: Board -> [(Int, Int)]
emptyEntries board = filter (isEmpty . getElem') [(i, j) | i <- [1..boardSize]
                                                         , j <- [1..boardSize]]
  where boardSize           = (boxSize board) ^ 2
        getElem' (row, col) = getElem row col $ mat board

-- This function returns True if the board is complete.
-- i.e. this function returns True if all entries in the board
-- are Full.
complete :: Board -> Bool
complete board = noFull board == (boxSize board)^4

-- This function returns the intersection of a list of sets.
intersectAll :: (Eq a) => [[a]] -> [a]
intersectAll [] = []
intersectAll xs = foldr1 intersect xs
