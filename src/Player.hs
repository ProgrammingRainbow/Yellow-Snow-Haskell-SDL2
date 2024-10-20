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
    let (SDL.Rectangle _ (SDL.V2 w h), _, _) = gamePlayerRect gameState
    let xF = (screenWidth - fromIntegral w) / 2
    let playerRect =
            ( SDL.Rectangle (SDL.P (SDL.V2 (round xF) (round playerY))) (SDL.V2 w h)
            , xF
            , playerY
            )

    put $
        gameState
            { gamePlayerRect = playerRect
            }

updatePlayer :: StateT GameState IO ()
updatePlayer = do
    gameState <- get
    keyboardState <- SDL.getKeyboardState
    let playerRect = gamePlayerRect gameState
        oldFlip = gamePlayerFlip gameState
        deltaTime = gameDeltaTime gameState

    let left = keyboardState SDL.ScancodeLeft || keyboardState SDL.ScancodeA
    let right = keyboardState SDL.ScancodeRight || keyboardState SDL.ScancodeD

    let (SDL.Rectangle (SDL.P (SDL.V2 _ y)) (SDL.V2 w h), oldXF, yF) = playerRect

    let playerDelta = playerVel * deltaTime

    let (newXF, newFlip)
            | left
                && not right
                && oldXF - playerDelta + playerLeftOffset > 0 =
                (oldXF - playerDelta, True)
            | right
                && not left
                && oldXF + playerDelta + fromIntegral w - playerRightOffset < screenWidth =
                (oldXF + playerDelta, False)
            | otherwise = (oldXF, oldFlip)

    let newPlayerRect =
            ( SDL.Rectangle (SDL.P (SDL.V2 (round newXF) y)) (SDL.V2 w h)
            , newXF
            , yF
            )
    put
        gameState
            { gamePlayerRect = newPlayerRect
            , gamePlayerFlip = newFlip
            }
