{-# LANGUAGE CPP, ForeignFunctionInterface #-}
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


-- This is a work around for the Prelude defined getChar function on
-- Windows. The Prelude defined function does not return the character
-- until the enter key is pressed. This function returns it immediately.
getChar' :: IO Char
#ifdef mingw32_HOST_OS
getChar' = liftM (chr.fromEnum) c_getch
-- Import the getch function on Windows.
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#else
getChar' = getChar
#endif

