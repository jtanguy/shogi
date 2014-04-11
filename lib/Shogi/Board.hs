{-#LANGUAGE OverloadedStrings #-}
module Shogi.Board where

import Data.Monoid
import qualified Data.Text as T

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
printBoard :: Board -> T.Text
printBoard = T.intercalate "\n" . map (T.concat . map showSq)

showSq :: Square -> T.Text
showSq = maybe "  " showPiece
  where
    showPiece (King c) = colored c " k"
    showPiece (Gold c) = colored c " g"
    showPiece (Rook p c) = promoted p <> colored c "r"
    showPiece (Bishop p c) = promoted p <> colored c "b"
    showPiece (Silver p c) = promoted p <> colored c "s"
    showPiece (Knight p c) = promoted p <> colored c "n"
    showPiece (Lance p c) = promoted p <> colored c "l"
    showPiece (Pawn p c) = promoted p <> colored c "p"
    promoted p = if p == Promoted then "+" else " "
    colored c = if c == Black then T.toUpper else T.toLower


