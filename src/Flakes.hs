module Flakes (
    resetFlake,
    updateFlakes,
) where

import           Control.Monad.State (StateT, get, liftIO, put)
import           Foreign.C.Types     (CInt)
import qualified SDL
import           System.Random       (randomRIO)

import           GameConfig          (flakeFloor, flakeVel, screenHeight,
                                      screenWidth)
import           GameTypes           (GameState (..))

resetFlake :: Bool -> SDL.Rectangle CInt -> IO (SDL.Rectangle CInt)
resetFlake full (SDL.Rectangle _ (SDL.V2 w h)) = do
    x <- randomRIO (0, screenWidth - w)
    y <-
        if full
            then randomRIO (-h, 2 * (-screenHeight))
            else randomRIO (-h, -screenHeight - h)

    return (SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h))

updateFlake :: SDL.Rectangle CInt -> IO (SDL.Rectangle CInt)
updateFlake flakeRect = do
    let SDL.Rectangle (SDL.P (SDL.V2 x y)) flakeDim = flakeRect

    if y + flakeVel > flakeFloor
        then resetFlake False flakeRect
        else return $ SDL.Rectangle (SDL.P (SDL.V2 x (y + flakeVel))) flakeDim

updateFlakes :: StateT GameState IO ()
updateFlakes = do
    gameState <- get
    whiteRects <- liftIO $ mapM updateFlake (gameWhiteRects gameState)
    yellowRects <- liftIO $ mapM updateFlake (gameYellowRects gameState)

    put $
        gameState
            { gameWhiteRects = whiteRects
            , gameYellowRects = yellowRects
            }
