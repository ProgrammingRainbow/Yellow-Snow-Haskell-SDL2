module GameTypes (
    GameData (..),
    GameState (..),
    initialGameState,
) where

import           Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer

import           GameConfig      (scoreX, scoreY)

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
    , gamePlayerRect  :: SDL.Rectangle CInt
    , gamePlayerFlip  :: Bool
    , gameWhiteRects  :: [SDL.Rectangle CInt]
    , gameYellowRects :: [SDL.Rectangle CInt]
    , gameScore       :: CInt
    , gameScoreSurf   :: Maybe SDL.Surface
    , gameScoreText   :: Maybe SDL.Texture
    , gameScoreRect   :: SDL.Rectangle CInt
    , gamePlaying     :: Bool
    }

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gamePlayerRect = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 0 0)
        , gamePlayerFlip = False
        , gameWhiteRects = []
        , gameYellowRects = []
        , gameScore = 0
        , gameScoreSurf = Nothing
        , gameScoreText = Nothing
        , gameScoreRect = SDL.Rectangle (SDL.P (SDL.V2 scoreX scoreY)) (SDL.V2 0 0)
        , gamePlaying = True
        }
