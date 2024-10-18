module InitSDL (
    initSDL,
) where

import           Control.Monad.State (StateT)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

import           GameConfig          (myAudioConfig, myWindowConfig,
                                      windowTitle)
import           GameTypes
import           GameUtils           (addClean, safeRun)

initSDL :: StateT GameState IO (SDL.Window, SDL.Renderer)
initSDL = do
    addClean $ putStrLn "All Clean!"

    safeRun
        SDL.initializeAll
        "Error initializing SDL2"
    addClean SDL.quit

    safeRun
        (SDL.Image.initialize [SDL.Image.InitPNG])
        "Error initializing SDL2 Image"
    addClean SDL.Image.quit

    safeRun
        SDL.Font.initialize
        "Error initializing SDL2 Font"
    addClean SDL.Font.quit

    safeRun
        (SDL.Mixer.initialize [SDL.Mixer.InitOGG])
        "Error initializing SDL2 Mixer"
    addClean SDL.Mixer.quit

    safeRun
        (SDL.Mixer.openAudio myAudioConfig 1024)
        "Error initializing SDL2 Mixer"
    addClean SDL.Mixer.closeAudio

    window <-
        safeRun
            (SDL.createWindow windowTitle myWindowConfig)
            "Error creating Window"
    addClean $ SDL.destroyWindow window

    renderer <-
        safeRun
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            "Error creating Renderer"
    addClean $ SDL.destroyRenderer renderer

    icon <-
        safeRun
            (SDL.Image.load "images/yellow.png")
            "Error loading Surface"
    SDL.setWindowIcon window icon
    SDL.freeSurface icon

    return (window, renderer)
