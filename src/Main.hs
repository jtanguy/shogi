
module Main where

import Data.Monoid
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta
import Shogi.Board

initialSFEN = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL"

main :: IO ()
main = case parseString board mempty initialSFEN of
    Failure xs -> putDoc xs
    Success b -> printBoard b
