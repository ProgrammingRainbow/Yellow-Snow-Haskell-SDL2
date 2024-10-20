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
    frameDelay,
    frameDelayMax,
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

screenWidth, screenHeight :: Float
screenWidth = 800
screenHeight = 600

fontColor :: SDL.Font.Color
fontColor = SDL.V4 255 255 255 255

fontSize :: Int
fontSize = 24

scoreX, scoreY :: CInt
scoreX = 10
scoreY = 10

flakeVel :: Float
flakeVel = 300

flakeFloor :: Float
flakeFloor = 514

frameDelay :: Float
frameDelay = 1000 / 60
frameDelayMax :: Float
frameDelayMax = 1000 / 60

playerVel, playerY, playerTopOffset, playerLeftOffset, playerRightOffset :: Float
playerVel = 300
playerY = 377
playerTopOffset = 10
playerLeftOffset = 47
playerRightOffset = 41

myWindowConfig :: SDL.WindowConfig
myWindowConfig =
    SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered
        , SDL.windowInitialSize =
            SDL.V2
                (round screenWidth)
                (round screenHeight)
        }

myAudioConfig :: SDL.Mixer.Audio
myAudioConfig =
    SDL.Mixer.defaultAudio
        { SDL.Mixer.audioFrequency = 44100
        }
