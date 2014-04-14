{-#LANGUAGE TemplateHaskell#-}
module Shogi.Game where

import Control.Lens
import Shogi.Board (Board,Color(..),Piece(..))

data Game = Game { _board :: Board
                 , _player :: Color
                 , _captured :: [Piece]
                 , _moveNum :: Maybe Integer
                 } deriving (Show, Eq)
$(makeLenses ''Game)

data Move = Move (Int,Int) (Int,Int)
          | Para (Int,Int)
          deriving (Show, Eq)

togglePlayer :: Color -> Color
togglePlayer Black = White
togglePlayer White = Black
