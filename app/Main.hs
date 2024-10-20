import           Control.Monad.State (evalStateT)

import           Game                (gameLoop, initGame)
import           GameTypes
import           InitSDL             (initSDL)
import           LoadMedia           (loadMedia)

main :: IO ()
main = do
    evalStateT (initSDL >>= loadMedia >>= initGame >>= gameLoop) initialGameState
