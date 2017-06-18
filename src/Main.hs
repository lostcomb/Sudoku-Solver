{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Sudoku
import Parser
import Solver

import Data.Char
import System.IO
import Control.Monad
import Foreign.C.Types

-- Main loop; asks for new Sudoku boards to solve.
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
        displaySolutions [board]

-- Allows the user to ask for more solutions.
displaySolutions :: [Board] -> IO ()
displaySolutions boards = case solve boards of
  Just (solution, boards') -> do
    putStr . show $ solution
    c <- getChar'
    case c of
      ';'       -> do putStrLn ""
                      displaySolutions boards'
      otherwise -> return ()
  Nothing                  -> putStrLn "No solution."


getChar' :: IO Char
getChar' = liftM (chr.fromEnum) c_getch

foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
