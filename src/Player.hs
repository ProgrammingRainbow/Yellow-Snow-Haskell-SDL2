module Player (
    resetPlayer,
    updatePlayer,
) where

import           Control.Monad.State (StateT, get, put)
import qualified SDL

import           GameConfig          (playerLeftOffset, playerRightOffset,
                                      playerVel, playerY, screenWidth)
import           GameTypes           (GameState (..))

resetPlayer :: StateT GameState IO ()
resetPlayer = do
    gameState <- get
    let (SDL.Rectangle _ (SDL.V2 w h)) = gamePlayerRect gameState
    let x = div (screenWidth - w) 2
    put $
        gameState
            { gamePlayerRect = SDL.Rectangle (SDL.P (SDL.V2 x playerY)) (SDL.V2 w h)
            }

updatePlayer :: StateT GameState IO ()
updatePlayer = do
    gameState <- get
    keyboardState <- SDL.getKeyboardState

    let left = keyboardState SDL.ScancodeLeft || keyboardState SDL.ScancodeA
    let right = keyboardState SDL.ScancodeRight || keyboardState SDL.ScancodeD

    let SDL.Rectangle (SDL.P (SDL.V2 oldX y)) (SDL.V2 w h) = gamePlayerRect gameState
        oldFlip = gamePlayerFlip gameState

    let (newX, newFlip)
            | left
                && not right
                && oldX - playerVel + playerLeftOffset > 0 =
                (oldX - playerVel, True)
            | right
                && not left
                && oldX + playerVel + w - playerRightOffset < screenWidth =
                (oldX + playerVel, False)
            | otherwise = (oldX, oldFlip)

    let playerRect = SDL.Rectangle (SDL.P (SDL.V2 newX y)) (SDL.V2 w h)
    put
        gameState
            { gamePlayerRect = playerRect
            , gamePlayerFlip = newFlip
            }
