module Audio
  ( Audio
  , initAudio
  , configureAudio
  , deconfigureAudio
  , audioSample
  , audioSampleBatch
  ) where

import Control.Monad.Trans
import Data.IORef
import Foreign
import Sound.ALSA.PCM
import qualified Sound.Frame.Stereo as Stereo

import Libretro

type Audio = IORef (Maybe (SoundSink Pcm (Stereo.T Int16), Pcm (Stereo.T Int16)))

initAudio :: IO Audio
initAudio = newIORef Nothing

configureAudio :: Audio -> Int -> IO ()
configureAudio audioRef rate = readIORef audioRef >>= configureAudio'
  where
    configureAudio' (Just (sink', handle')) = do
      soundSinkClose sink' handle'
      configureAudio' Nothing
    configureAudio' Nothing = do
      let sink = alsaSoundSink "default" (SoundFmt rate)
      handle <- soundSinkOpen sink
      writeIORef audioRef (Just (sink, handle))

deconfigureAudio :: Audio -> IO ()
deconfigureAudio audioRef = readIORef audioRef >>= deconfigureAudio'
  where
    deconfigureAudio' Nothing = return ()
    deconfigureAudio' (Just (sink, handle)) = do
      soundSinkClose sink handle
      writeIORef audioRef Nothing

audioSample :: Audio -> Int16 -> Int16 -> RetroM ()
audioSample audioRef left right = liftIO $ readIORef audioRef >>= audioSample'
  where
    audioSample' Nothing = return ()
    audioSample' (Just (sink, handle)) = do
      withArray [Stereo.cons left right] $ \ptr -> soundSinkWrite sink handle ptr 2

audioSampleBatch :: Audio -> Ptr Int16 -> Word32 -> RetroM Word64
audioSampleBatch audioRef ptr frames = liftIO $ readIORef audioRef >>= audioSampleBatch'
  where
    audioSampleBatch' Nothing = return 0
    audioSampleBatch' (Just (sink, handle)) = do
      soundSinkWrite sink handle (castPtr ptr) (fromIntegral frames)
      return (fromIntegral frames)
