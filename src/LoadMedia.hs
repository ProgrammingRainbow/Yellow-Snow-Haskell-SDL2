module LoadMedia (
    loadMedia,
) where

import           Control.Monad.State (StateT, liftIO, modify, replicateM)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

import           Flakes              (resetFlake)
import           GameConfig          (fontSize)
import           GameTypes
import           GameUtils           (addClean, rectFromTexture, safeRun)
import           Player              (resetPlayer)
import           Score               (updateScore)

loadMedia :: (SDL.Window, SDL.Renderer) -> StateT GameState IO GameData
loadMedia (window, renderer) = do
    background <-
        safeRun
            (SDL.Image.loadTexture renderer "images/background.png")
            "Error loading Texture"
    addClean $ SDL.destroyTexture background

    font <-
        safeRun
            (SDL.Font.load "fonts/freesansbold.ttf" fontSize)
            "Error creating Font"
    addClean $ SDL.Font.free font

    player <-
        safeRun
            (SDL.Image.loadTexture renderer "images/player.png")
            "Error loading a Texture"
    addClean $ SDL.destroyTexture player

    whiteFlake <-
        safeRun
            (SDL.Image.loadTexture renderer "images/white.png")
            "Error loading a Texture"
    addClean $ SDL.destroyTexture whiteFlake

    yellowFlake <-
        safeRun
            (SDL.Image.loadTexture renderer "images/yellow.png")
            "Error loading a Texture"
    addClean $ SDL.destroyTexture yellowFlake

    playerRect <-
        safeRun
            (rectFromTexture player)
            "Error querying Texture"

    whiteRect <-
        safeRun
            (rectFromTexture whiteFlake)
            "Error querying Texture"
    whiteRects <- liftIO $ replicateM 10 (resetFlake True whiteRect)

    yellowRect <-
        safeRun
            (rectFromTexture yellowFlake)
            "Error querying Texture"
    yellowRects <- liftIO $ replicateM 5 (resetFlake True yellowRect)

    modify $
        \gameState ->
            gameState
                { gamePlayerRect = playerRect
                , gameWhiteRects = whiteRects
                , gameYellowRects = yellowRects
                }

    resetPlayer

    collectSound <-
        safeRun
            (SDL.Mixer.load "sounds/collect.ogg")
            "Error loading Chunk"
    addClean $ SDL.Mixer.free collectSound

    hitSound <-
        safeRun
            (SDL.Mixer.load "sounds/hit.ogg")
            "Error loading Chunk"
    addClean $ SDL.Mixer.free hitSound

    music <-
        safeRun
            (SDL.Mixer.load "music/winter_loop44100.ogg")
            "Error loading Music"
    addClean $ SDL.Mixer.free music

    addClean $ SDL.Mixer.halt SDL.Mixer.AllChannels
    addClean SDL.Mixer.haltMusic

    safeRun
        (SDL.Mixer.playMusic SDL.Mixer.Forever music)
        "Error Playing Music"

    let gameData =
            GameData
                { gameWindow = window
                , gameRenderer = renderer
                , gameFont = font
                , gameBackground = background
                , gamePlayer = player
                , gameWhiteFlake = whiteFlake
                , gameYellowFlake = yellowFlake
                , gameCollectSound = collectSound
                , gameHitSound = hitSound
                , gameMusic = music
                }

    updateScore gameData

    return gameData
