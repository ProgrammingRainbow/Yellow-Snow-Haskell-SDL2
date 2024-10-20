module GameFPS (
    fpsToggle,
    fpsPrint,
    fpsDelay,
) where

import           Control.Monad.State (StateT, get, liftIO, modify, put, when)
import qualified SDL

import           GameConfig          (frameDelay, frameDelayMax)
import           GameTypes

fpsToggle :: StateT GameState IO ()
fpsToggle = do
    fpsTime <- SDL.ticks
    modify $
        \gameState ->
            gameState
                { gameFpsTime = fpsTime
                , gameFpsCount = 1
                , gameFpsEnable = not (gameFpsEnable gameState)
                }

fpsPrint :: StateT GameState IO ()
fpsPrint = do
    gameState <- get

    when (gameFpsEnable gameState) $ do
        let lastTime = gameFpsTime gameState
            count = gameFpsCount gameState

        currentTime <- SDL.ticks

        let timeElapsed
                | currentTime >= lastTime = currentTime - lastTime
                | otherwise = maxBound - lastTime + currentTime + 1

        let (newTime, newCount)
                | timeElapsed >= 1000 && currentTime < lastTime = (lastTime + 1001, 1)
                | timeElapsed >= 1000 = (lastTime + 1000, 1)
                | otherwise = (lastTime, count + 1)

        when (timeElapsed >= 1000) $ liftIO $ print count

        put
            gameState
                { gameFpsTime = newTime
                , gameFpsCount = newCount
                }

fpsDelay :: StateT GameState IO ()
fpsDelay = do
    gameState <- get
    let lastTime = gameLastTime gameState
        carryDelay = gameCarryDelay gameState

    currentTime <- do
        firstTime <- SDL.ticks
        if firstTime >= lastTime
            then do
                let elapsedTime = fromIntegral (firstTime - lastTime)
                if (frameDelay + carryDelay) > elapsedTime
                    then do
                        let delay = frameDelay - elapsedTime + carryDelay
                        SDL.delay $ round delay
                        SDL.ticks
                    else return firstTime
            else return firstTime

    let newElapsedTime
            | currentTime >= lastTime = fromIntegral (currentTime - lastTime)
            | otherwise = 0

    let newCarryDelay
            | currentTime >= lastTime = frameDelay - newElapsedTime + carryDelay
            | otherwise = 0

    let newCarryDelay'
            | newCarryDelay > frameDelayMax = frameDelayMax
            | newCarryDelay < (-frameDelayMax) = -frameDelayMax
            | otherwise = newCarryDelay

    let deltaTime = newElapsedTime / 1000

    put
        gameState
            { gameLastTime = currentTime
            , gameCarryDelay = newCarryDelay'
            , gameDeltaTime = deltaTime
            }

    fpsPrint
