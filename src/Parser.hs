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

lexer = makeTokenParser emptyDef

parseBoard :: String -> Either String Board
parseBoard str = case parse (whiteSpace lexer *> boardParser <* eof) "" str of
  (Left  e) -> Left $ show e
  (Right r) -> Right r

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

rowParser :: Parser [Entry]
rowParser = entryParser `sepBy1` comma lexer

entryParser :: Parser Entry
entryParser =     fullParser
              <|> emptyParser
              <?> invalidEntryError

fullParser :: Parser Entry
fullParser = Full . fromIntegral <$> integer lexer

emptyParser :: Parser Entry
emptyParser = Sudoku.Empty <$ dot lexer

invalidEntryError :: String
invalidEntryError = "Invalid entry - entries should be of the form '1', '2', ...,n or '.'."

invalidSqrtError :: String
invalidSqrtError = "The size of the specified board does not have an integer square root."

invalidSizeError :: String
invalidSizeError = "The specified board is not square."
