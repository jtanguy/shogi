
module Main where

import Control.Monad.Trans.RWS
import Control.Monad.Trans.RWS
import Data.Monoid
import qualified Data.Text.IO as TIO
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta (parseString, Result(..))
import UI.NCurses

import Shogi.Board
import Shogi.Game
import Shogi.SFEN
import Game.UI

-- initialSFEN = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL B -"
initialSFEN = "7r1/6B1p/6Bsk/9/7P1/9/9/9/9 B 2S"

main :: IO ()
main = case parseString sfen mempty initialSFEN of
    Failure xs -> putDoc xs
    Success g -> runCurses $ do
        setEcho False
        w <- defaultWindow
        _ <- runRWST (mainLoop) w (UIState (g) (0,0))
        return ()

