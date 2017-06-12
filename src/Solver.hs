module Solver where

import Sudoku

{-
  This module solves the given Sudoku board.
-}

solve :: Board -> Maybe Board
solve = undefined

-- Need to build up a tree of boards, each time checking for solution and duplicates.

-- Iterate: 1. Build new level of search tree.
--          2. Prune all boards that are not valid.
--          Repeat until solution is found - may be cool to
--          keep outputting solutions until no more can be found. (like Prolog)
