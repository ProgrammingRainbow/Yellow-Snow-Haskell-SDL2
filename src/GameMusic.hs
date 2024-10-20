module GameMusic (
    startMusic,
    toggleMusic,
) where

import           Control.Monad.State (StateT, get, gets, put, when)
import qualified SDL.Mixer

import           GameTypes
import           GameUtils           (safeRun)

startMusic :: SDL.Mixer.Music -> StateT GameState IO ()
startMusic music = do
    musicEnable <- gets gameMusicEnable

    when musicEnable $ do
        safeRun
            (SDL.Mixer.playMusic SDL.Mixer.Forever music)
            "Error Playing Music"

toggleMusic :: SDL.Mixer.Music -> StateT GameState IO ()
toggleMusic music = do
    gameState <- get
    let musicEnable = not $ gameMusicEnable gameState
        playing = gamePlaying gameState

    put $
        gameState
            { gameMusicEnable = musicEnable
            }

    when playing $ do
        if musicEnable
            then startMusic music
            else SDL.Mixer.haltMusic
