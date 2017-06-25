# Sudoku Solver

This program will solve a given Sudoku board. The given board can be of
any size so long as it is square and the size has an integer square root.
This program treats Sudoku as a board game and builds up a game tree to
find the solution. The game tree search is optimised using some of the
rules a person would use when solving the puzzles.

Extra constraints can also be given. These can be either:

* regions that should have unique values (the same constraints as
applied to the rows, columns and boxes).
* regions whose sum should equal a specified value (these regions must also contain unique values).

## Compilation

To compile, run: `stack build`.

To execute, run: `stack exec sudoku-solver`.

## Mathematical Solutions

Another method of solving the Sudoku puzzle is to use binary integer linear
programming. This method is outlined [here](http://langvillea.people.cofc.edu/sudoku5.pdf).

Yet another method of solving the Sudoku puzzle is to formulate the puzzle
in terms of the Vertex Colouring problem. This method is outlined [here](http://www.ams.org/notices/200706/tx070600708p.pdf).

## Licence

Please refer to the `LICENCE` file.
