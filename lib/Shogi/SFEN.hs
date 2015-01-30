module Shogi.SFEN where

import Control.Applicative
import Data.Char
import Text.Trifecta

import Shogi.Board
import Shogi.Game

-- * Parsing
sfen :: Parser Game
sfen = Game <$> (boardParser <* char ' ') <*> (color <* char ' ') <*> hand <*> optional (char ' ' *> natural) <* eof

color :: Parser Color
color = (char 'B' *> pure Black)
    <|> (char 'W' *> pure White)

hand :: Parser [Piece]
hand = (char '-' *> pure [])
    <|> fmap concat (many pieces)

pieces :: Parser [Piece]
pieces = replicate <$> option 1 num <*> piece

boardParser :: Parser Board
boardParser = sepBy boardLine (char '/')

boardLine :: Parser [Square]
boardLine = fmap concat $ many ( ((:[]). Just <$> piece) <|> emptySq )
  where
    emptySq = flip replicate Nothing <$> num

num :: Parser Int
num = fmap f $ oneOf "123456789"
  where
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f _ = 9


piece :: Parser Piece
piece = choice $ map try [king,gold,rook,bishop,silver,knight,lance,pawn]

king :: Parser Piece
king = King . colored <$> oneOf "kK"

gold :: Parser Piece
gold = Gold . colored <$> oneOf "gG"

rook :: Parser Piece
rook = Rook <$> promoted <*> fmap colored (oneOf "rR")

bishop :: Parser Piece
bishop = Bishop <$> promoted <*> fmap colored (oneOf "bB")

silver :: Parser Piece
silver = Silver <$> promoted <*> fmap colored (oneOf "sS")

knight :: Parser Piece
knight = Knight <$> promoted <*> fmap colored (oneOf "nN")

lance :: Parser Piece
lance = Lance <$> promoted <*> fmap colored (oneOf "lL")

pawn :: Parser Piece
pawn = Pawn <$> promoted <*> fmap colored (oneOf "pP")

-- ** Utility
colored :: Char -> Color
colored c | isUpper c = Black
          | otherwise = White

promoted :: Parser Promotion
promoted = option Plain (try $ char '+' *> pure Promoted)
