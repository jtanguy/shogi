
module Shogi.Board where

import Control.Applicative
import Data.Char
import Data.List
import Text.Trifecta

data Promotion = Plain | Promoted deriving (Show, Eq)

data Color = Black | White deriving (Show, Eq)

data Piece = King Color -- ^King
           | Rook Promotion Color -- ^Rook
           | Bishop Promotion Color -- ^Bishop
           | Gold Color -- ^Gold general
           | Silver Promotion Color -- ^Silver General
           | Knight Promotion Color -- ^Knight
           | Lance Promotion Color -- ^Lance
           | Pawn Promotion Color -- ^Pawn
           deriving (Show, Eq)

type Square = Maybe Piece

type Board = [[Square]]

-- * Printing
printBoard :: Board -> IO ()
printBoard = mapM_ $ putStrLn . concat . map showSq
  where
    showSq = maybe " _" showPiece
    showPiece (King c) = map (if c == Black then toUpper else id) " k"
    showPiece (Gold c) = map (if c == Black then toUpper else id) " g"
    showPiece (Rook p c) = (if p == Promoted then "+" else " ") ++ map (if c == Black then toUpper else id) "r"
    showPiece (Bishop p c) = (if p == Promoted then "+" else " ") ++ map (if c == Black then toUpper else id) "b"
    showPiece (Silver p c) = (if p == Promoted then "+" else " ") ++ map (if c == Black then toUpper else id) "s"
    showPiece (Knight p c) = (if p == Promoted then "+" else " ") ++ map (if c == Black then toUpper else id) "n"
    showPiece (Lance p c) = (if p == Promoted then "+" else " ") ++ map (if c == Black then toUpper else id) "l"
    showPiece (Pawn p c) = (if p == Promoted then "+" else " ") ++ map (if c == Black then toUpper else id) "p"


-- * Parsing
board :: Parser Board
board = sepBy boardLine (char '/') <* eof

boardLine :: Parser [Square]
boardLine = fmap concat $ many $ ((:[]). Just <$> piece) <|> emptySq
  where
    emptySq = (flip replicate Nothing). fromInteger <$> natural

piece :: Parser Piece
piece = choice $ map try [king,gold,rook,bishop,silver,knight,lance,pawn]

king :: Parser Piece
king = King . colored <$> oneOf "kK"

gold :: Parser Piece
gold = Gold . colored <$> oneOf "gG"

rook :: Parser Piece
rook = Rook <$> promoted <*> (fmap colored $ oneOf "rR")

bishop :: Parser Piece
bishop = Bishop <$> promoted <*> (fmap colored $ oneOf "bB")

silver :: Parser Piece
silver = Silver <$> promoted <*> (fmap colored $ oneOf "sS")

knight :: Parser Piece
knight = Knight <$> promoted <*> (fmap colored $ oneOf "nN")

lance :: Parser Piece
lance = Lance <$> promoted <*> (fmap colored $ oneOf "lL")

pawn :: Parser Piece
pawn = Pawn <$> promoted <*> (fmap colored $ oneOf "pP")

-- * Utility
colored :: Char -> Color
colored c | isUpper c = Black
          | otherwise = White

promoted :: Parser Promotion
promoted = option Plain (try $ char '+' *> pure Promoted)
