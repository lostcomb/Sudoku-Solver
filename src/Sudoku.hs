module Sudoku where

import Data.List

{-
  This module provides the data types to represent the state of the Sudoku game.
  This module provides the functions to help check for valid board states.
-}

-- The data type for the entries in the Sudoku board.
data Entry
  = Full Int
  | Empty
  deriving (Eq)

-- Print the correct character for each constructor.
instance Show Entry where
  show (Full i) = show i
  show Empty    = "."

-- Allow us to manipulate entries.
instance Num Entry where
  x        + Empty    = x
  Empty    + x        = x
  (Full i) + (Full j) = Full (i + j)

  x        * Empty    = Empty
  Empty    * x        = Empty
  (Full i) * (Full j) = Full (i * j)

  abs Empty    = Empty
  abs (Full x) = Full (abs x)

  signum Empty    = Empty
  signum (Full x) = Full (signum x)

  fromInteger 0 = Empty
  fromInteger x = Full (fromIntegral x)

  negate Empty    = Empty
  negate (Full x) = Full (negate x)

-- Allow us to generate entries using list comprehension.
instance Enum Entry where
  toEnum 0 = Empty
  toEnum x = Full x

  fromEnum Empty    = 0
  fromEnum (Full x) = x

-- This function returns True if the specified Entry is Full.
isFull :: Entry -> Bool
isFull (Full _) = True
isFull _        = False

-- This function returns True if the specified Entry is Empty.
isEmpty :: Entry -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Type synonyms to make better use of the type system.
type Set = [Entry]
type Row = Set

-- The data type for constraints that valid solutions must
-- satisfy.
data Constraint
  = Unique  [(Int, Int)]
  | Sum Int [(Int, Int)]
  deriving (Show, Eq, Read)

-- The data type for the Sudoku board. This data type contains
-- the size of the boxes, and the rows of the board. It also
-- contains user-defined constraints on the solutions.
data Board = Board Int [Row] [Constraint]
  deriving (Eq)

-- This function returns the size of the specified board.
boardSize :: Board -> Int
boardSize (Board _ rows _) = length rows

-- This function returns the size of the boxes of the specified
-- board.
boxSize :: Board -> Int
boxSize (Board n _ _) = n

-- Pretty print the Sudoku board.
instance Show Board where
  show (Board n rows _) = init
                        . unlines
                        . concat
                        . map (\box -> [lineStr] ++ box ++ [lineStr])
                        . map (intersperse lineStr)
                        . chunk n
                        . map rowStr $ rows
    where p             = length . show $ (n * n)
          rowStr        = concat
                        . map (\box -> "| " ++ box ++ " |")
                        . map (intercalate " | ")
                        . chunk n
                        . map (pad p . show)
          lineStr       = concat
                        . replicate (length rows `div` n) $ boxStr
          boxStr        = "+" ++ replicate (n * (p + 2) + (n - 1)) '-' ++ "+"
          pad n str     = replicate (n - length str) ' ' ++ str

-- This function returns the entry at the specified index of the
-- specified board.
getEntry :: Board -> Int -> Int -> Entry
getEntry (Board _ rows _) row_i col_i
  | outOfBounds = Empty
  | otherwise   = entry
  where row         = rows !! row_i
        entry       = row !! col_i
        n2          = length rows
        outOfBounds =  row_i >= n2 || col_i >= n2
                    || row_i <  0  || col_i <  0


-- This function returns the list of rows of the specified board.
getRows :: Board -> [Set]
getRows (Board _ rows _) = rows

-- This function returns the list of columns of the specified board.
getColumns :: Board -> [Set]
getColumns (Board _ rows _) = transpose rows

-- This function returns the list of boxes of the specified board.
getBoxes :: Board -> [Set]
getBoxes (Board n rows _) = boxes
  where rows' = chunk n . map (chunk n) $ rows
        boxes = concat . map zipN $ rows'
        zipN :: [[[a]]] -> [[a]]
        zipN xs
          | (sum . map length) xs == 0 = []
          | otherwise                  = box : zipN xs'
          where box = concat . map head $ xs
                xs' = map tail xs

-- This function returns the constraints of the specified board.
getConstraints :: Board -> [Constraint]
getConstraints (Board _ _ cs) = cs

-- This function returns the possible values for the specified entry.
-- If the specified entry already has a value, said value is the only
-- value returned.
validEntries :: Board -> Int -> Int -> [Entry]
validEntries board row_i col_i
  | outOfBounds  = []
  | isFull entry = [entry]
  | otherwise    = intersectAll ([ possibleValuesUnique size row
                                 , possibleValuesUnique size col
                                 , possibleValuesUnique size box
                                 ] ++ constraints)
  where size        = boardSize board
        b_size      = boxSize board
        entry       = (getRows board !! row_i) !! col_i
        row         = getRows    board !! row_i
        col         = getColumns board !! col_i
        box         = getBoxes   board !! box_i
        box_i       = (row_i `div` b_size) * b_size + (col_i `div` b_size)
        n2          = length (getRows board)
        outOfBounds =  row_i >= n2 || col_i >= n2
                    || row_i <  0  || col_i <  0
        constraints = map (cToV board size) . filter (elemConstraint (row_i, col_i)) . getConstraints $ board

cToV :: Board -> Int -> Constraint -> [Entry]
cToV board n (Unique cs) = possibleValuesUnique n (map (uncurry $ getEntry board) cs)
cToV board _ (Sum  s cs) = possibleValuesSum    s (map (uncurry $ getEntry board) cs)

elemConstraint :: (Int, Int) -> Constraint -> Bool
elemConstraint x (Unique cs) = elem x cs
elemConstraint x (Sum  _ cs) = elem x cs

-- This function returns the possible entries for a given set
-- constrained by the uniqueness property.
possibleValuesUnique :: Int -> Set -> [Entry]
possibleValuesUnique n set = filter ((flip notElem) set) [Full 1..Full n]

-- This function returns the possible entries for a given set
-- constrained by the summation property (i.e. all entries must
-- add up to the specified value).
possibleValuesSum :: Int -> Set -> [Entry]
possibleValuesSum target set
  | empties == 1 = [tar_val]
  | otherwise    = [Full 1..tar_val]
  where empties  = length . filter (==Empty) $ set
        curr_sum = sum set
        tar_val  = fromInteger (fromIntegral target) - curr_sum

-- This function returns the coordinates of all empty entries in the Sudoku
-- board.
emptyEntries :: Board -> [(Int, Int)]
emptyEntries (Board _ rows _) = coordinates 0 indices
  where indices = map (findIndices (==Empty)) rows
        coordinates _   [] = []
        coordinates row is =  map (\col -> (row, col)) (head is)
                           ++ coordinates (row + 1) (tail is)

-- This function updates the entry at the specified (0-index) position
-- in the board. If the specified position is off the board, this function
-- makes no change to the board.
updateEntry :: Board -> Int -> Int -> Entry -> Board
updateEntry (Board n rows cs) row_i col_i entry
  | outOfBounds = Board n rows  cs
  | otherwise   = Board n rows' cs
  where row         = rows !! row_i
        row'        = update col_i entry row
        rows'       = update row_i row' rows
        n2          = length rows
        outOfBounds =  row_i >= n2 || col_i >= n2
                    || row_i <  0  || col_i <  0

-- This function returns True if the board is complete.
-- i.e. this function returns True if all entries in the board
-- are Full.
complete :: Board -> Bool
complete (Board _ rows _) = and . map (and . map isFull) $ rows

-- This function returns a uniform 9x9 board.
uniform9x9Board :: Entry -> Board
uniform9x9Board = uniformBoard 3

-- This function returns a square board of size n^2 with all entries the same.
uniformBoard :: Int -> Entry -> Board
uniformBoard n e = Board n ((replicate n2 . replicate n2) e) []
  where n2 = n * n

-- This function returns a square board of size n^2 with entries in ascending
-- order.
nonUniformBoard :: Int -> Board
nonUniformBoard n
  = Board n [[Full (row_i * n2 + col_i) | col_i <- [0..n2]] | row_i <- [0..n2]] []
  where n2 = (n * n) - 1

-- This function updates the element at the specified index of the list.
-- If the specified position is <= 0, the element is appended to the head
-- of the list. If the specified position is >= length of the list, the
-- element appended to the end of the list.
update :: Int -> a -> [a] -> [a]
update index x xs = h_xs ++ x:t_xs
  where h_xs = take index xs
        t_xs = drop (index + 1) xs

-- This function returns a list containing the contiguous chunks of size
-- n of the specified list. The last chunk can be of size less than n.
chunk :: Int -> [a] -> [[a]]
chunk n xs
  | length xs <= n = [xs]
  | otherwise      = take n xs : chunk n (drop n xs)

-- This function returns the intersection of a list of sets.
intersectAll :: (Eq a) => [[a]] -> [a]
intersectAll = foldr1 intersect
