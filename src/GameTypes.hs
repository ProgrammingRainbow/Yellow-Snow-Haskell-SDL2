module GameTypes (
    RectWithFloats,
    GameData (..),
    GameState (..),
    initialGameState,
) where

import           Data.Word       (Word32)
import           Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer

import           GameConfig      (scoreX, scoreY)

type RectWithFloats = (SDL.Rectangle CInt, Float, Float)

data GameData = GameData
    { gameWindow       :: SDL.Window
    , gameRenderer     :: SDL.Renderer
    , gameFont         :: SDL.Font.Font
    , gameBackground   :: SDL.Texture
    , gamePlayer       :: SDL.Texture
    , gameWhiteFlake   :: SDL.Texture
    , gameYellowFlake  :: SDL.Texture
    , gameCollectSound :: SDL.Mixer.Chunk
    , gameHitSound     :: SDL.Mixer.Chunk
    , gameMusic        :: SDL.Mixer.Music
    }

data GameState = GameState
    { gameActions     :: [IO ()]
    , gamePlayerRect  :: RectWithFloats
    , gamePlayerFlip  :: Bool
    , gameWhiteRects  :: [RectWithFloats]
    , gameYellowRects :: [RectWithFloats]
    , gameScore       :: CInt
    , gameScoreSurf   :: Maybe SDL.Surface
    , gameScoreText   :: Maybe SDL.Texture
    , gameScoreRect   :: SDL.Rectangle CInt
    , gamePlaying     :: Bool
    , gameMusicEnable :: Bool
    , gameLastTime    :: Word32
    , gameCarryDelay  :: Float
    , gameDeltaTime   :: Float
    , gameFpsTime     :: Word32
    , gameFpsCount    :: Word32
    , gameFpsEnable   :: Bool
    }

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gamePlayerRect = (SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 0 0), 0, 0)
        , gamePlayerFlip = False
        , gameWhiteRects = []
        , gameYellowRects = []
        , gameScore = 0
        , gameScoreSurf = Nothing
        , gameScoreText = Nothing
        , gameScoreRect = SDL.Rectangle (SDL.P (SDL.V2 scoreX scoreY)) (SDL.V2 0 0)
        , gamePlaying = True
        , gameMusicEnable = True
        , gameLastTime = 0
        , gameCarryDelay = 0
        , gameDeltaTime = 0
        , gameFpsTime = 0
        , gameFpsCount = 0
        , gameFpsEnable = False
        }
