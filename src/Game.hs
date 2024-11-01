module Game (
    resetGame,
    gameLoop,
    initGame,
) where

import           Control.Monad       (replicateM, unless, when)
import           Control.Monad.State (StateT, get, gets, liftIO, modify, put)
import qualified SDL
import qualified SDL.Mixer

import           Collisions          (checkCollisions)
import           Flakes              (resetFlake, updateFlakes)
import           GameFPS             (fpsDelay, fpsToggle)
import           GameMusic           (startMusic, toggleMusic)
import           GameTypes
import           GameUtils           (exitClean, rectFromTexture, safeRun)
import           Player              (resetPlayer, updatePlayer)
import           Score               (updateScore)

initGame :: GameData -> StateT GameState IO GameData
initGame gameData = do
    let player = gamePlayer gameData
        whiteFlake = gameWhiteFlake gameData
        yellowFlake = gameYellowFlake gameData
        music = gameMusic gameData

    playerRect <-
        safeRun
            (rectFromTexture player)
            "Error querying Texture"

    whiteRect <-
        safeRun
            (rectFromTexture whiteFlake)
            "Error querying Texture"
    whiteRects <- liftIO $ replicateM 10 (resetFlake True (whiteRect, 0, 0))

    yellowRect <-
        safeRun
            (rectFromTexture yellowFlake)
            "Error querying Texture"
    yellowRects <- liftIO $ replicateM 5 (resetFlake True (yellowRect, 0, 0))

    safeRun
        (SDL.Mixer.playMusic SDL.Mixer.Forever music)
        "Error Playing Music"

    modify $
        \gameState ->
            gameState
                { gamePlayerRect = (playerRect, 0, 0)
                , gameWhiteRects = whiteRects
                , gameYellowRects = yellowRects
                }

    resetPlayer
    updateScore gameData

    return gameData

resetGame :: GameData -> StateT GameState IO ()
resetGame gameData = do
    gameState <- get

    let oldWhiteRects = gameWhiteRects gameState
        oldYellowRects = gameYellowRects gameState
        music = gameMusic gameData

    whiteRects <- liftIO $ mapM (resetFlake True) oldWhiteRects
    yellowRects <- liftIO $ mapM (resetFlake True) oldYellowRects

    put $
        gameState
            { gameScore = 0
            , gameWhiteRects = whiteRects
            , gameYellowRects = yellowRects
            , gamePlaying = True
            }

    updateScore gameData

    startMusic music

handleEvents :: GameData -> [SDL.Event] -> StateT GameState IO ()
handleEvents _ [] = return ()
handleEvents gameData (event : events) = do
    playing <- gets gamePlaying

    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean
                    SDL.KeycodeSpace  -> unless playing $ resetGame gameData
                    SDL.KeycodeF      -> fpsToggle
                    SDL.KeycodeM      -> toggleMusic $ gameMusic gameData
                    _                 -> return ()
        SDL.QuitEvent -> exitClean
        _ -> return ()
    handleEvents gameData events

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    SDL.pollEvents >>= handleEvents gameData

    playing <- gets gamePlaying
    when playing $ do
        updateFlakes
        updatePlayer
        checkCollisions gameData

    let renderer = gameRenderer gameData
        background = gameBackground gameData
        whiteFlake = gameWhiteFlake gameData
        yellowFlake = gameYellowFlake gameData
        player = gamePlayer gameData

    gameState <- get
    let (playerRect, _, _) = gamePlayerRect gameState
        playerFlip = gamePlayerFlip gameState
        whiteRects = map (\(rect, _, _) -> rect) (gameWhiteRects gameState)
        yellowRects = [rect | (rect, _, _) <- gameYellowRects gameState]
        scoreText = gameScoreText gameState
        scoreRect = gameScoreRect gameState

    SDL.clear renderer

    SDL.copy renderer background Nothing Nothing
    SDL.copyEx renderer player Nothing (Just playerRect) 0 Nothing (SDL.V2 playerFlip False)
    mapM_ (SDL.copy renderer whiteFlake Nothing . Just) whiteRects
    mapM_ (SDL.copy renderer yellowFlake Nothing . Just) yellowRects
    mapM_ (\s -> SDL.copy renderer s Nothing $ Just scoreRect) scoreText

    SDL.present renderer

    fpsDelay

    gameLoop gameData
