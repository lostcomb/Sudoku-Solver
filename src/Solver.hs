module Solver where

import Sudoku

import Data.List

{-
  This module solves the given Sudoku board.
-}

-- This function returns a solution to the specified
-- board or Nothing if one does not exist. It also returns
-- the working list of boards, minus the solution, to allow
-- alternative solutions to be found.
solve :: [Board] -> Maybe (Board, [Board])
solve []     = Nothing
solve agenda = case solution agenda of
  Just board -> Just board
  Nothing    -> solve (nub . nextStepDF $ agenda)

-- This function returns the specified agenda with the next level of
-- the head of the agenda applied. This implements depth-first search.
nextStepDF :: [Board] -> [Board]
nextStepDF (x:xs) = x' ++ xs
  where x' = minimumLength . nextLevel $ x

-- This function returns the specified agenda with the next level of
-- the head of the agenda applied. This implements breadth-first search.
nextStepBF :: [Board] -> [Board]
nextStepBF (x:xs) = xs ++ x'
  where x' = minimumLength . nextLevel $ x

-- This function returns the next level of the game tree for the
-- specified Sudoku board.
nextLevel :: Board -> [[Board]]
nextLevel board = map (\(row, col) -> map (updateEntry  board row col)
                                          (validEntries board row col))
                $ emptyEntries board

-- This function returns Nothing if the head of the agenda is not
-- a solution. It returns the head of the list and the tail of the
-- list if the head of the list is a solution.
solution :: [Board] -> Maybe (Board, [Board])
solution [] = Nothing
solution (x:xs)
  | complete x = Just (x, xs)
  | otherwise  = Nothing

-- This function returns the element of the specified list with minimum
-- length.
minimumLength :: (Foldable t) => [t a] -> t a
minimumLength = minimumBy (\a b -> compare (length a) (length b))
