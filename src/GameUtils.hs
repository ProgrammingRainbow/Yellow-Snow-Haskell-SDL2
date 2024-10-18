module GameUtils (
    addClean,
    exitClean,
    safeRun,
    playChunk,
    dimFromSurface,
    rectFromTexture,
) where

import           Control.Exception   (SomeException, catch)
import           Control.Monad.State (StateT, get, liftIO, modify)
import           Foreign.C.Types     (CInt)
import qualified SDL
import qualified SDL.Mixer
import           System.Exit         (exitFailure, exitSuccess)
import           System.IO           (hPrint, hPutStrLn, stderr)

import           GameTypes           (GameState (..))

addClean :: IO () -> StateT GameState IO ()
addClean action = do
    modify (\gameState -> gameState{gameActions = action : gameActions gameState})

cleanAll :: GameState -> IO ()
cleanAll gameState = do
    let actions = gameActions gameState
    liftIO $ sequence_ actions
    mapM_ SDL.freeSurface $ gameScoreSurf gameState
    mapM_ SDL.destroyTexture $ gameScoreText gameState

exitClean :: StateT GameState IO ()
exitClean = do
    gameState <- get
    liftIO $ cleanAll gameState
    liftIO exitSuccess

errorClean :: GameState -> String -> SomeException -> IO a
errorClean gameState errorMsg e = do
    liftIO $ hPutStrLn stderr $ errorMsg ++ ":"
    liftIO $ hPrint stderr e
    liftIO $ cleanAll gameState
    liftIO exitFailure

safeRun :: IO a -> String -> StateT GameState IO a
safeRun action errorMsg = do
    gameState <- get
    liftIO $ catch action $ errorClean gameState errorMsg

playChunk :: SDL.Mixer.Chunk -> IO ()
playChunk chunk = do
    maybeChannel <- SDL.Mixer.getAvailable SDL.Mixer.DefaultGroup
    case maybeChannel of
        Just channel -> do
            _ <- SDL.Mixer.playOn channel 1 chunk
            return ()
        Nothing -> return ()

dimFromSurface :: SDL.Surface -> IO (SDL.V2 CInt)
dimFromSurface surface = do
    SDL.V2 surfaceW surfaceH <- SDL.surfaceDimensions surface
    return $ SDL.V2 surfaceW surfaceH

rectFromTexture :: SDL.Texture -> IO (SDL.Rectangle CInt)
rectFromTexture texture = do
    SDL.TextureInfo
        { SDL.textureWidth = textureWidth
        , SDL.textureHeight = textureHeight
        } <-
        SDL.queryTexture texture
    return $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 textureWidth textureHeight)
