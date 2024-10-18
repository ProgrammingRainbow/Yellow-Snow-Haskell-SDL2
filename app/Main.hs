import           Control.Monad.State (StateT, evalStateT, get, gets, liftIO,
                                      put, unless, when)
import qualified SDL
import qualified SDL.Mixer

import           Collision           (checkCollisions)
import           Flakes              (resetFlake, updateFlakes)
import           GameTypes
import           GameUtils           (exitClean, safeRun)
import           InitSDL             (initSDL)
import           LoadMedia           (loadMedia)
import           Player              (updatePlayer)
import           Score               (updateScore)

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

    safeRun
        (SDL.Mixer.playMusic SDL.Mixer.Forever music)
        "Error Playing Music"

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
    let playerRect = gamePlayerRect gameState
        playerFlip = gamePlayerFlip gameState
        whiteRects = gameWhiteRects gameState
        yellowRects = gameYellowRects gameState
        scoreText = gameScoreText gameState
        scoreRect = gameScoreRect gameState

    SDL.clear renderer

    SDL.copy renderer background Nothing Nothing
    SDL.copyEx renderer player Nothing (Just playerRect) 0 Nothing (SDL.V2 playerFlip False)
    mapM_ (SDL.copy renderer whiteFlake Nothing . Just) whiteRects
    mapM_ (SDL.copy renderer yellowFlake Nothing . Just) yellowRects
    mapM_ (\s -> SDL.copy renderer s Nothing $ Just scoreRect) scoreText

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    evalStateT (initSDL >>= loadMedia >>= gameLoop) initialGameState
