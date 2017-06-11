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
    full :- '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    empty :- '.'
-}

lexer = makeTokenParser emptyDef

parseBoard :: String -> Either String Board
parseBoard str = case parse (whiteSpace lexer *> boardParser <* eof) "" str of
  (Left  e) -> Left $ show e
  (Right r) -> Right r

boardParser :: Parser Board
boardParser = do board <- rowParser `sepBy1` semi lexer
                 let n      = length (head board)
                     sqrt_n = floor . sqrt . fromIntegral $ n
                     rowLen = foldr (\row acc -> length row == n && acc)
                                    True
                                    board

                 if rowLen && length board == n
                   then return $ Board sqrt_n Row board
                   else if sqrt_n * sqrt_n /= n
                   then unexpected "The size of the specified board does not have an integer square root."
                   else unexpected "The specified board is not square."

rowParser :: Parser [Entry]
rowParser = entryParser `sepBy1` comma lexer

entryParser :: Parser Entry
entryParser =     fullParser
              <|> emptyParser
              <?> "Invalid entry - entries should be of the form '1', '2', ..., '9' or '.'."

fullParser :: Parser Entry
fullParser = Full <$> oneToNine

oneToNine :: Parser Int
oneToNine = do i <- integer lexer
               if i < 1 || i > 9
                 then fail "Invalid integer - 1-9 only."
                 else return . fromIntegral $ i

emptyParser :: Parser Entry
emptyParser = Sudoku.Empty <$ dot lexer
