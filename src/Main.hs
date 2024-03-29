{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Main where

import Sudoku
import Parser
import Solver

import Data.Char
import System.IO
import System.Exit
import Control.Monad
import Foreign.C.Types
import System.Environment
import System.Console.ANSI
import System.Console.Terminal.Size

-- Main loop; asks for new Sudoku boards to solve.
main :: IO ()
main = do
  putStrLn "Sudoku Solver (c) Julian Loscombe 2017."
  forever $ do
    putStr prompt
    hFlush stdout
    boardStr <- getLine
    case parseBoard boardStr of
      (Left  error_ops   ) -> do
        clearInput boardStr
        putStr prompt
        sequence_ error_ops
      (Right Nothing     ) -> do
        exitSuccess
      (Right (Just board)) -> do
        displaySolutions [board]

-- This function clears the input string so that the error
-- message can be displayed in its place.
clearInput :: String -> IO ()
clearInput str = do
  s <- size
  return ()
  case s of
    Just window -> do
      let noLines = ceiling $ fromIntegral (length prompt + length str)
                            / fromIntegral (width window)
      replicateM_ noLines $ do
        clearLine
        cursorUp 1
    Nothing     -> return ()

-- Allows the user to ask for more solutions.
displaySolutions :: [Board] -> IO ()
displaySolutions boards = case solve boards of
  Just (solution, boards') -> do
    putStr . show $ solution
    hFlush stdout
    c <- getChar'
    case c of
      ';'       -> do putStrLn " ;"
                      putStrLn ""
                      displaySolutions boards'
      otherwise -> do putStrLn ""
  Nothing                  -> do
    setSGR [SetColor Foreground Dull Red]
    putStrLn "EoS."
    setSGR [Reset]

-- This defines the console prompt.
prompt :: String
prompt = "*>"

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

