module Main
    ( main
    ) where

import Test.Tasty (defaultMain, testGroup)

import qualified Shogi.SFEN.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Shogi.SFEN.Tests.tests
    ]

