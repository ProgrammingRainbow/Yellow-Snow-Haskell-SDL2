module Flakes (
    resetFlake,
    updateFlakes,
) where

import           Control.Monad.State (StateT, get, liftIO, put)
import qualified SDL
import           System.Random       (randomRIO)

import           GameConfig          (flakeFloor, flakeVel, screenHeight,
                                      screenWidth)
import           GameTypes           (GameState (..), RectWithFloats)

resetFlake :: Bool -> RectWithFloats -> IO RectWithFloats
resetFlake full (SDL.Rectangle _ (SDL.V2 w h), _, _) = do
    x <- randomRIO (0, screenWidth - fromIntegral w)
    y <-
        if full
            then randomRIO (-(fromIntegral h), 2 * (-screenHeight))
            else randomRIO (-(fromIntegral h), -screenHeight - fromIntegral h)

    return (SDL.Rectangle (SDL.P (SDL.V2 (round x) (round y))) (SDL.V2 w h), x, y)

updateFlake :: Float -> RectWithFloats -> IO RectWithFloats
updateFlake deltaTime flakeRect = do
    let (SDL.Rectangle (SDL.P (SDL.V2 x _)) flakeDim, xF, yF) = flakeRect
        newYF = yF + flakeVel * deltaTime

    if newYF > flakeFloor
        then resetFlake False flakeRect
        else return (SDL.Rectangle (SDL.P (SDL.V2 x (round newYF))) flakeDim, xF, newYF)

updateFlakes :: StateT GameState IO ()
updateFlakes = do
    gameState <- get
    let deltaTime = gameDeltaTime gameState

    whiteRects <- liftIO $ mapM (updateFlake deltaTime) (gameWhiteRects gameState)
    yellowRects <- liftIO $ mapM (updateFlake deltaTime) (gameYellowRects gameState)

    put $
        gameState
            { gameWhiteRects = whiteRects
            , gameYellowRects = yellowRects
            }
