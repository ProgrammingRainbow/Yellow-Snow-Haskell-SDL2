module Score (
    updateScore,
    incrementScore,
) where

import           Control.Monad.State (StateT, get, modify, put)
import           Data.Text           (pack)
import qualified SDL
import qualified SDL.Font

import           GameConfig          (fontColor, scoreX, scoreY)
import           GameTypes
import           GameUtils           (dimFromSurface, safeRun)

updateScore :: GameData -> StateT GameState IO ()
updateScore gameData = do
    gameState <- get
    let score = gameScore gameState
        renderer = gameRenderer gameData
        font = gameFont gameData

    mapM_ SDL.destroyTexture $ gameScoreText gameState
    mapM_ SDL.freeSurface $ gameScoreSurf gameState

    let message = pack ("Score: " ++ show score)

    scoreSurf <-
        safeRun
            (SDL.Font.blended font fontColor message)
            "Error creating Surface from Font"

    scoreText <-
        safeRun
            (SDL.createTextureFromSurface renderer scoreSurf)
            "Error creating Texture from Surface"

    scoreDim <-
        safeRun
            (dimFromSurface scoreSurf)
            "Error creating Rectange from Surface"

    put $
        gameState
            { gameScoreSurf = Just scoreSurf
            , gameScoreText = Just scoreText
            , gameScoreRect = SDL.Rectangle (SDL.P (SDL.V2 scoreX scoreY)) scoreDim
            }

incrementScore :: GameData -> StateT GameState IO ()
incrementScore gameData = do
    modify $ \gameState -> gameState{gameScore = 1 + gameScore gameState}
    updateScore gameData
