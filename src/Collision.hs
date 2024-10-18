module Collision (
    checkCollisions,
) where

import           Control.Monad.State (StateT, get, liftIO, put, when)
import           Foreign.C.Types     (CInt)
import qualified SDL
import qualified SDL.Mixer

import           Flakes              (resetFlake)
import           GameConfig          (playerLeftOffset, playerRightOffset,
                                      playerTopOffset, playerY)
import           GameTypes
import           GameUtils           (playChunk)
import           Score               (incrementScore)

checkCollision :: SDL.Rectangle CInt -> SDL.Rectangle CInt -> Bool
checkCollision playerRect flakeRect = do
    let SDL.Rectangle (SDL.P (SDL.V2 flakeX flakeY)) (SDL.V2 flakeW flakeH) = flakeRect
    let SDL.Rectangle (SDL.P (SDL.V2 playerX _)) (SDL.V2 playerW _) = playerRect

    flakeY + flakeH > playerY + playerTopOffset
        && flakeX + flakeW > playerX + playerLeftOffset
        && flakeX < playerX + playerW - playerRightOffset

checkCollisions :: GameData -> StateT GameState IO ()
checkCollisions gameData = do
    gameState <- get

    let playerRect = gamePlayerRect gameState
        whiteRects = gameWhiteRects gameState
        yellowRects = gameYellowRects gameState
        collectSound = gameCollectSound gameData
        hitSound = gameHitSound gameData

    newWhiteRects <-
        mapM
            ( \rect -> do
                collide <- liftIO . pure $ checkCollision playerRect rect
                if collide
                    then do
                        liftIO $ playChunk collectSound
                        incrementScore gameData
                        liftIO $ resetFlake False rect
                    else return rect
            )
            whiteRects

    yellowCollide <- liftIO . pure $ any (checkCollision playerRect) yellowRects

    when yellowCollide $ do
        liftIO $ playChunk hitSound
        liftIO SDL.Mixer.haltMusic

    updateState <- get
    put $
        updateState
            { gameWhiteRects = newWhiteRects
            , gamePlaying = not yellowCollide
            }
