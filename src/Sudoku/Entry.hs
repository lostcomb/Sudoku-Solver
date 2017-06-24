module Sudoku.Entry where

-- The data type for the entries in the Sudoku board.
data Entry
  = Full Int
  | Empty
  deriving (Eq)

-- This function returns True if the specified Entry is Full.
isFull :: Entry -> Bool
isFull (Full _) = True
isFull _        = False

-- This function returns True if the specified Entry is Empty.
isEmpty :: Entry -> Bool
isEmpty Empty = True
isEmpty _     = False

-- Print the correct character for each constructor.
instance Show Entry where
  show (Full i) = show i
  show Empty    = "."

-- Allow us to manipulate entries as numbers.
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
