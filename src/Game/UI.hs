{-#LANGUAGE OverloadedStrings #-}
module Game.UI where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Data.Text()
import UI.NCurses

import Shogi.Board
import Shogi.Game
import Game.Util

data UIState = UIState { _game :: Game, _current :: (Integer,Integer)}

game :: Lens' UIState Game
game = lens _game (\uis g -> uis { _game = g })

current :: Lens' UIState (Integer,Integer)
current = lens _current (\uis c -> uis { _current = c })

drawGame :: Game -> Update ()
drawGame g = do
    mapM_ (\(i,line) -> moveCursor i 0 >> drawText line) $
        zip [0..] ["  9  8  7  6  5  4  3  2  1 "
                  ,"┌┈┈┬┈┈┬┈┈┬┈┈┬┈┈┬┈┈┬┈┈┬┈┈┬┈┈┐"
                  ,"│  │  │  │  │  │  │  │  │  │ a"
                  ,"├┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ b"
                  ,"├┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ c"
                  ,"├┈┈┼┈┈┼┈┈∙┈┈┼┈┈┼┈┈∙┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ d"
                  ,"├┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ e"
                  ,"├┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ f"
                  ,"├┈┈┼┈┈┼┈┈∙┈┈┼┈┈┼┈┈∙┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ g"
                  ,"├┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ h"
                  ,"├┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┼┈┈┤"
                  ,"│  │  │  │  │  │  │  │  │  │ i"
                  ,"└┈┈┴┈┈┴┈┈┴┈┈┴┈┈┴┈┈┴┈┈┴┈┈┴┈┈┘"]
    updateBoard (g^.board)
    moveCursor 20 0
    drawText "Current player: "
    drawString (g^.player.to show)
    maybe (return ()) (\n -> do
        drawText " (Move n°"
        drawString (show n)
        drawText ")"
        ) (g^.moveNum)

updateBoard :: Board -> Update ()
updateBoard b = forM_ [0..8] $ \i -> (forM_ [0..8] $ \j -> do
    moveCursor (2*i+2) (3*j+1)
    drawText (showSq ((b!!(fromInteger i))!!(fromInteger j)))
    )

update :: RWST Window [Move] UIState Curses ()
update = do
    w <- ask
    g <- gets (view (game))
    (ci,cj) <- gets (view current)
    lift $ updateWindow w (drawGame g >> moveCursor (2*ci+2) (3*cj+2))
    lift $ render

mainLoop :: RWST Window [Move] UIState Curses ()
mainLoop = loop where
    loop = do
        update
        w <- ask
        ev <- lift $ getEvent w Nothing
        case ev of
            Nothing -> loop
            Just (EventCharacter k) -> case k of
                'q' -> return ()
                ' ' -> modify (game.player %~ togglePlayer) >> loop
                _ -> loop
            Just (EventSpecialKey k) -> case k of
                KeyUpArrow	    -> modify (current %~ (mapFst $ inBounds . subtract 1)) >> loop
                KeyDownArrow    -> modify (current %~ (mapFst $ inBounds . (1+))) >> loop
                KeyLeftArrow	-> modify (current %~ (mapSnd $ inBounds . subtract 1)) >> loop
                KeyRightArrow   -> modify (current %~ (mapSnd $ inBounds . (1+))) >> loop
                _ -> loop
            _ -> loop
    inBounds = (`mod` 9) . (9+)
