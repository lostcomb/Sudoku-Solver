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

    board :- {{entry ','}^n-1 entry ';'}^n-1 ({entry ','}^n-1 entry)
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
boardParser
  = do board <- rowParser `sepBy1` semi lexer
       let n       = length (head board)
           sqrt_n  = floor . sqrt . fromIntegral $ n
           rowLen  = and . map ((==n) . length) $ board
           bounded = and . map (and . map (lte n)) $ board
           lte :: Int -> Entry -> Bool
           lte n Sudoku.Empty = True
           lte n (Full i)     = i <= n

       if rowLen && length board == n then return $ Board sqrt_n board
       else if sqrt_n * sqrt_n /= n   then unexpected invalidSqrtError
       else if not bounded            then unexpected invalidEntryError
       else                                unexpected invalidSizeError

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

-- Error message for an invalid entry.
invalidEntryError :: String
invalidEntryError = "Invalid entry - entries should be of the form '1', '2', ...,n or '.'."

-- Error message for a board of an invalid size.
invalidSqrtError :: String
invalidSqrtError = "The size of the specified board does not have an integer square root."

-- Error message for a board that is not square.
invalidSizeError :: String
invalidSizeError = "The specified board is not square."
