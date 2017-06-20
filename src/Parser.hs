module Parser where

import Sudoku

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Control.Applicative (pure, (<$), (<$>), (<*), (<*>), (*>))

{-
  This module provides the parser for initial board sudoku board states.

  The format for the board is as follows:

    board :- partial_board | full_board
    partial_board :- integer 'x' integer ';' [entries]
    entries :- loc_entry | loc_entry ',' entries
    loc_entry :- '(' integer ',' integer ',' entry ')'
    full_board :- {{entry ','}^n-1 entry ';'}^n-1 ({entry ','}^n-1 entry)
    entry :- full | empty
    full :- '1' | '2' | '3' | ... | n
    empty :- '.'
-}

-- Lexer for the grammar.
lexer = makeTokenParser emptyDef

-- Parse a description of a Sudoku board.
parseBoard :: String -> Either String Board
parseBoard str = case parse (whiteSpace lexer *> boardParser <* eof) "" str of
  (Left  e) -> Left $ show e
  (Right r) -> Right r

-- Parse a description of a Sudoku board and perform some validation
-- to check that the board is square, it's size has an integer square root
-- (so that the board can be split into boxes) and check that each entry is
-- and integer in the range 1 - size of the board, or empty (.).
boardParser :: Parser Board
boardParser =   try partial_boardParser
            <|> full_boardParser
            <?> invalidFormat

-- Parse a board that is described by locations where entries occur.
-- This is useful when the board does not contain many full entries.
partial_boardParser :: Parser Board
partial_boardParser
  = do size <- intParser
       let box_s = floor . sqrt . fromIntegral $ size
       semi lexer
       entries <- loc_entryParser `sepBy` comma lexer
       if box_s * box_s /= size
         then unexpected invalidSqrtError
         else return $ foldr (\(row, col, entry) acc
                               -> updateEntry acc row col entry)
                             (uniformBoard box_s Sudoku.Empty)
                             entries

-- Parse a tuple containing the row and column indices and the entry.
loc_entryParser :: Parser (Int, Int, Entry)
loc_entryParser = parens lexer $ (,,) <$> intParser
                                      <*  comma lexer
                                      <*> intParser
                                      <*  comma lexer
                                      <*> entryParser

-- Parse an Int using the Integer parser.
intParser :: Parser Int
intParser = fromIntegral <$> integer lexer

-- Parse a board that is described by each entry in turn.
-- This is useful when the board contains a lot of full entries.
full_boardParser :: Parser Board
full_boardParser
  = do board <- rowParser `sepBy1` semi lexer
       let n       = length (head board)
           sqrt_n  = floor . sqrt . fromIntegral $ n
           rowLen  = and . map ((==n) . length) $ board
           bounded = and . map (and . map (lte n)) $ board
           lte :: Int -> Entry -> Bool
           lte n Sudoku.Empty = True
           lte n (Full i)     = i <= n && i > 0


       if sqrt_n * sqrt_n /= n             then unexpected invalidSqrtError
       else if not bounded                 then unexpected invalidEntryError
       else if rowLen && length board == n then return $ Board sqrt_n board
       else                                     unexpected invalidSizeError

-- Parse a row of the Sudoku board.
rowParser :: Parser [Entry]
rowParser = entryParser `sepBy1` comma lexer

-- Parse an entry of the Sudoku board.
entryParser :: Parser Entry
entryParser =     fullParser
              <|> emptyParser
              <?> invalidEntryError

-- Parse an entry that contains an integer.
fullParser :: Parser Entry
fullParser = Full . fromIntegral <$> integer lexer

-- Parse an entry that is empty.
emptyParser :: Parser Entry
emptyParser = Sudoku.Empty <$ dot lexer

-- Error message for an invalid entry format.
invalidFormat :: String
invalidFormat = "Invalid entry format, either specify the whole board, or the size and locations of each entry."

-- Error message for an invalid entry.
invalidEntryError :: String
invalidEntryError = "Invalid entry - entries should be of the form '1', '2', ...,n or '.'."

-- Error message for a board of an invalid size.
invalidSqrtError :: String
invalidSqrtError = "The size of the specified board does not have an integer square root."

-- Error message for a board that is not square.
invalidSizeError :: String
invalidSizeError = "The specified board is not square."
