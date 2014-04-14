module Shogi.SFEN.Tests where

import Control.Lens
import Data.Monoid
import Text.Trifecta
import Test.Tasty
import Test.Tasty.HUnit

import Shogi.Game
import Shogi.Board
import Shogi.SFEN

tests :: TestTree
tests = testGroup "SFEN" [
    testGroup "Parser" [
        testCase "Initial board" initialTest
        , testCase "Sample board" sample1Test
        ]
    ]

initialTest :: Assertion
initialTest = parseString sfen mempty initialSFEN ^? _Success @?= Just initialGame


initialSFEN :: String
initialSFEN = "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL B -"

initialGame :: Game
initialGame = Game [ [ Just (Lance Plain White),Just (Knight Plain White), Just (Silver Plain White),Just (Gold White), Just (King White)
                     , Just (Gold White), Just (Silver Plain White),Just (Knight Plain White),Just (Lance Plain White)]
                   , [ Nothing, Just (Rook Plain White), Nothing, Nothing, Nothing, Nothing, Nothing, Just (Bishop Plain White), Nothing]
                   , [Just (Pawn Plain White), Just (Pawn Plain White), Just (Pawn Plain White), Just (Pawn Plain White), Just (Pawn Plain White)
                     , Just (Pawn Plain White), Just (Pawn Plain White), Just (Pawn Plain White), Just (Pawn Plain White)]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Just (Pawn Plain Black), Just (Pawn Plain Black), Just (Pawn Plain Black), Just (Pawn Plain Black), Just (Pawn Plain Black)
                     , Just (Pawn Plain Black), Just (Pawn Plain Black), Just (Pawn Plain Black), Just (Pawn Plain Black)]
                   , [ Nothing, Just (Bishop Plain Black), Nothing, Nothing, Nothing, Nothing, Nothing, Just (Rook Plain Black), Nothing]
                   , [ Just (Lance Plain Black),Just (Knight Plain Black), Just (Silver Plain Black),Just (Gold Black), Just (King Black)
                     , Just (Gold Black), Just (Silver Plain Black),Just (Knight Plain Black),Just (Lance Plain Black)]
                   ] Black [] Nothing

sample1Test :: Assertion
sample1Test = parseString sfen mempty sample1SFEN ^? _Success @?= Just sample1Game

sample1SFEN :: String
sample1SFEN = "7r1/6B1p/6Bsk/9/7P1/9/9/9/9 B 2S"

sample1Game :: Game
sample1Game = Game [ [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Rook Plain White), Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Bishop Plain Black), Nothing, Just (Pawn Plain White)]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Bishop Plain Black), Just (Silver Plain White),Just (King White)]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just (Pawn Plain Black), Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                   ] Black [Silver Plain Black, Silver Plain Black] Nothing
