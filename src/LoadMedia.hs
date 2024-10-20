module LoadMedia (
    loadMedia,
) where

import           Control.Monad.State (StateT)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

import           GameConfig          (fontSize)
import           GameTypes
import           GameUtils           (addClean, safeRun)

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

    return gameData
