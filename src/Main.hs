module Main where

import Sudoku
import Parser
import Solver

import System.IO
import Control.Monad

-- Main loop, asks for new Sudoku boards to solve.
main :: IO ()
main = do
  putStrLn "Sudoku Solver (c) Julian Loscombe."
  forever $ do
    putStr "*>"
    hFlush stdout
    boardStr <- getLine
    case parseBoard boardStr of
      (Left  error) -> do
        putStrLn error
      (Right board) -> do
        --solution <- solve board
        --putStrLn . show $ solution
        putStrLn . show $ board
