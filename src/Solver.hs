module Solver where

import Sudoku

import Data.List

{-
  This module solves the given Sudoku board.
-}

solve :: Board -> Maybe Board
solve board = solve' [board]
  where solve' [] = Nothing
        solve' xs = case findSolution xs of
          Just board -> Just board
          Nothing    -> solve' (nub . generateNextLevel $ xs)

generateNextLevel :: [Board] -> [Board]
generateNextLevel boards = concat . map nextLevel $ boards
  where nextLevel board = concat
                        . map (\(row, col) -> map (updateEntry  board row col)
                                                  (validEntries board row col))
                        $ emptyEntries board

findSolution :: [Board] -> Maybe Board
findSolution [] = Nothing
findSolution (x:xs)
  | complete x = Just x
  | otherwise  = findSolution xs

--          Repeat until solution is found - may be cool to
--          keep outputting solutions until no more can be found. (like Prolog)
