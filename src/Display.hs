module Display where

import Graphics.UI.WX

ui :: IO ()
ui = do
  f    <- frame [text := "Sudoku Solver"]
  quit <- button f [text := "Quit", on command := close f]
  set f [layout := widget quit]
