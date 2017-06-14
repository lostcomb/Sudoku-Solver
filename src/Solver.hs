module Solver where

import Sudoku

import Data.List

{-
  This module solves the given Sudoku board.
-}

-- This function returns a solution to the specified
-- board or Nothing if one does not exist.
solve :: Board -> Maybe Board
solve board = solve' [board]
  where solve' [] = Nothing
        solve' xs = case findSolution xs of
          Just board -> Just board
          Nothing    -> solve' (nub . genMinLevel $ xs)

-- This function generates the next level of the full game
-- tree.
genFullLevel :: [Board] -> [Board]
genFullLevel boards = concat . map (concat . nextLevel) $ boards

-- This function generates the next level of a specialised
-- game tree whereby only the entry with the fewest possibilities
-- is used to create the next state.
genMinLevel :: [Board] -> [Board]
genMinLevel boards = concat
                   . map (minimumBy (\a b -> compare (length a) (length b)))
                   $ boards'
  where boards' = map nextLevel $ boards

-- This function returns the next level of the game tree for the
-- specified Sudoku board.
nextLevel :: Board -> [[Board]]
nextLevel board = map (\(row, col) -> map (updateEntry  board row col)
                                          (validEntries board row col))
                $ emptyEntries board

-- This function returns Nothing if the specified list of boards
-- does not contain a solution, or Just the solution if it does.
findSolution :: [Board] -> Maybe Board
findSolution [] = Nothing
findSolution (x:xs)
  | complete x = Just x
  | otherwise  = findSolution xs
