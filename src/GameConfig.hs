module GameConfig (
    windowTitle,
    screenWidth,
    screenHeight,
    fontColor,
    fontSize,
    scoreX,
    scoreY,
    flakeVel,
    flakeFloor,
    playerVel,
    playerY,
    playerTopOffset,
    playerLeftOffset,
    playerRightOffset,
    myWindowConfig,
    myAudioConfig,
) where

import           Data.Text       (Text, pack)
import           Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer

windowTitle :: Text
windowTitle = pack "Don't Eat the Yellow Snow!"

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

fontColor :: SDL.Font.Color
fontColor = SDL.V4 255 255 255 255

fontSize :: Int
fontSize = 24

scoreX, scoreY :: CInt
scoreX = 10
scoreY = 10

flakeVel :: CInt
flakeVel = 5

flakeFloor :: CInt
flakeFloor = 514

playerVel, playerY, playerTopOffset, playerLeftOffset, playerRightOffset :: CInt
playerVel = 5
playerY = 377
playerTopOffset = 10
playerLeftOffset = 47
playerRightOffset = 41

myWindowConfig :: SDL.WindowConfig
myWindowConfig =
    SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered
        , SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        }

myAudioConfig :: SDL.Mixer.Audio
myAudioConfig =
    SDL.Mixer.defaultAudio
        { SDL.Mixer.audioFrequency = 44100
        }
