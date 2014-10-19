#include "SDL_mixer.h"
module Graphics.UI.SDL.Mixer
  ( version
  , initFLAC
  , initMOD
  , initMODPLUG
  , initMP3
  , initOGG
  , initFLUIDSYNTH
  , init
  , quit
  , openAudio
  , allocateChannels
  , querySpec
  , loadWAVRW
  , loadMUS
  , loadMUSRW
  , loadMUSTypeRW
  , quickLoadWav
  , quickLoadRAW
  , getNumChunkDecoders
  , getChunkDecoder
  , getNumMusicDecoders
  , getMusicDecoder
  , getMusicType
  , setPanning
  , setPosition
  , setDistance
  , setReverseStereo
  , reserveChannels
  , groupChannel
  , groupChannels
  , groupAvailable
  , groupCount
  , groupOldest
  , groupNewer
  , playChannelTimed
  , playMusic
  , fadeInMusic
  , fadeInMusicPos
  , fadeInChannelTimed
  , volume
  , volumeChunk
  , volumeMusic
  , haltChannel
  , haltGroup
  , haltMusic
  , expireChannel
  , fadeOutChannel
  , fadeOutGroup
  , fadeOutMusic
  , fadingMusic
  , fadingChannel
  , pause
  , resume
  , paused
  , pauseMusic
  , resumeMusic
  , rewindMusic
  , pausedMusic
  , setMusicPosition
  , playing
  , playingMusic
  , setMusicCMD
  , setSynchroValue
  , getSynchroValue
  , setSoundFonts
  , getSoundFonts
  , getChunk
  , closeAudio
  ) where

import Foreign
import Foreign.C.String
import Control.Applicative
import Prelude hiding (init)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Audio (AudioFormat(..), fromAudioFormat, toAudioFormat)
import Graphics.UI.SDL.Error (getError)
import Graphics.UI.SDL.Mixer.Types

-- Error handling until something better is decided on in the main lib.
handleErrorI :: (Num a, Ord a) => String -> a -> (a -> Bool) -> (a -> IO b) -> IO b
handleErrorI fname i errCond fn
  | errCond i = (\err -> error $ fname ++ ": " ++ show err) =<< getError
  | otherwise = fn i
{-# INLINE handleErrorI #-}

-- | (Major, Minor, Patchlevel)
version :: (Int, Int, Int)
version = ( #{const SDL_MIXER_MAJOR_VERSION}
          , #{const SDL_MIXER_MINOR_VERSION}
          , #{const SDL_MIXER_PATCHLEVEL}
          )

newtype MixInitFlag
      = MixInitFlag { unwrapMixInitFlag :: #{type int} }

#{enum MixInitFlag, MixInitFlag
 , initFLAC = MIX_INIT_FLAC
 , initMOD = MIX_INIT_MOD
 , initMODPLUG = MIX_INIT_MODPLUG
 , initMP3 = MIX_INIT_MP3
 , initOGG = MIX_INIT_OGG
 , initFLUIDSYNTH = MIX_INIT_FLUIDSYNTH
 }

combineMixInitFlag :: [MixInitFlag] -> MixInitFlag
combineMixInitFlag = MixInitFlag . foldr ((.|.) . unwrapMixInitFlag) 0

foreign import ccall unsafe "Mix_Init"
  mixInit' :: #{type int} -> IO ()

init :: [MixInitFlag] -> IO ()
init flags =
  let flags' = unwrapMixInitFlag $ combineMixInitFlag flags
  in mixInit' flags'

foreign import ccall unsafe "Mix_Quit"
  quit :: IO ()

foreign import ccall unsafe "Mix_OpenAudio"
  mixOpenAudio' :: #{type int} -> #{type Uint16} -> #{type int} -> #{type int} -> IO #{type int}

openAudio :: Int -> AudioFormat -> Int -> Int -> IO ()
openAudio freq format channels chunksize = do
  let freq'      = fromIntegral freq
      format'    = fromAudioFormat format
      channels'  = fromIntegral channels
      chunksize' = fromIntegral chunksize
  ret <- mixOpenAudio' freq' format' channels' chunksize'
  handleErrorI "openAudio" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_AllocateChannels"
  mixAllocateChannels' :: #{type int} -> IO #{type int}

allocateChannels :: Int -> IO ()
allocateChannels numchans = do
  _ <- mixAllocateChannels' (fromIntegral numchans)
  return ()

foreign import ccall unsafe "Mix_QuerySpec"
  mixQuerySpec' :: Ptr #{type int} -> Ptr #{type Uint16} -> Ptr #{type int} -> IO #{type int}

querySpec :: IO (Int, AudioFormat, Int)
querySpec =
  alloca $ \freq' ->
  alloca $ \format' ->
  alloca $ \channels' -> do
    ret <- mixQuerySpec' freq' format' channels'
    handleErrorI "querySpec" ret (==0) $ \_ -> do
      freq     <- fmap fromIntegral $ peek freq'
      format   <- fmap toAudioFormat $ peek format'
      channels <- fmap fromIntegral $ peek channels'
      return (freq, format, channels)

foreign import ccall unsafe "Mix_LoadWAV_RW"
  mixLoadWAVRW' :: Ptr RWopsStruct -> #{type int} -> IO (Ptr ChunkStruct)

loadWAVRW :: RWops -> Bool -> IO Chunk
loadWAVRW rwops dofree =
  withForeignPtr rwops $ \rwops' ->
    mixLoadWAVRW' rwops' (fromBool dofree) >>= mkFinalizedChunk

foreign import ccall unsafe "Mix_LoadMUS"
  mixLoadMUS' :: CString -> IO (Ptr MusicStruct)

loadMUS :: String -> IO Music
loadMUS fname =
  withCString fname $ \fname' ->
    mixLoadMUS' fname' >>= mkFinalizedMusic

foreign import ccall unsafe "Mix_LoadMUS_RW"
  mixLoadMUSRW' :: Ptr RWopsStruct -> #{type int} -> IO (Ptr MusicStruct)

loadMUSRW :: RWops -> Bool -> IO Music
loadMUSRW rwops dofree =
  withForeignPtr rwops $ \rwops' ->
    mixLoadMUSRW' rwops' (fromBool dofree) >>= mkFinalizedMusic

foreign import ccall unsafe "Mix_LoadMUSType_RW"
  mixLoadMUSTypeRW' :: Ptr RWopsStruct -> #{type Mix_MusicType} -> #{type int} -> IO (Ptr MusicStruct)

loadMUSTypeRW :: RWops -> MusicType -> Bool -> IO Music
loadMUSTypeRW rwops mt dofree =
  withForeignPtr rwops $ \rwops' ->
    mixLoadMUSTypeRW' rwops' (musicTypeToConstant mt) (fromBool dofree) >>= mkFinalizedMusic

foreign import ccall unsafe "Mix_QuickLoad_WAV"
  mixQuickLoadWAV' :: Ptr #{type Uint8} -> IO (Ptr ChunkStruct)

quickLoadWav :: B.ByteString -> IO Chunk
quickLoadWav bs =
  let (bs'', _, _) = BI.toForeignPtr bs
  in withForeignPtr bs'' $ \bs' -> mixQuickLoadWAV' bs' >>= mkFinalizedChunk

foreign import ccall unsafe "Mix_QuickLoad_RAW"
  mixQuickLoadRAW' :: Ptr #{type Uint8} -> #{type Uint32} -> IO (Ptr ChunkStruct)

quickLoadRAW :: B.ByteString -> IO Chunk
quickLoadRAW bs =
  let (bs'', _, len) = BI.toForeignPtr bs
  in withForeignPtr bs'' $ \bs' -> mixQuickLoadRAW' bs' (fromIntegral len) >>= mkFinalizedChunk

foreign import ccall unsafe "Mix_GetNumChunkDecoders"
  mixGetNumChunkDecoders' :: IO #{type int}

getNumChunkDecoders :: IO Int
getNumChunkDecoders = fromIntegral <$> mixGetNumChunkDecoders'

foreign import ccall unsafe "Mix_GetChunkDecoder"
  mixGetChunkDecoder' :: #{type int} -> IO CString

getChunkDecoder :: Int -> IO String
getChunkDecoder index =
  mixGetChunkDecoder' (fromIntegral index) >>= peekCString

foreign import ccall unsafe "Mix_GetNumMusicDecoders"
  mixGetNumMusicDecoders' :: IO #{type int}

getNumMusicDecoders :: IO Int
getNumMusicDecoders = fromIntegral <$> mixGetNumMusicDecoders'

foreign import ccall unsafe "Mix_GetMusicDecoder"
  mixGetMusicDecoder' :: #{type int} -> IO CString

getMusicDecoder :: Int -> IO String
getMusicDecoder index =
  mixGetMusicDecoder' (fromIntegral index) >>= peekCString

foreign import ccall unsafe "Mix_GetMusicType"
  mixGetMusicType' :: Ptr MusicStruct -> IO #{type Mix_MusicType}

getMusicType :: Music -> IO MusicType
getMusicType music =
  withForeignPtr music $ \music' ->
    constantToMusicType <$> mixGetMusicType' music'

musicTypeToConstant :: MusicType -> #{type Mix_MusicType}
musicTypeToConstant NONE = #{const MUS_NONE}
musicTypeToConstant CMD = #{const MUS_CMD}
musicTypeToConstant WAV = #{const MUS_WAV}
musicTypeToConstant MOD = #{const MUS_MOD}
musicTypeToConstant MID = #{const MUS_MID}
musicTypeToConstant OGG = #{const MUS_OGG}
musicTypeToConstant MP3 = #{const MUS_MP3}
musicTypeToConstant MP3_MAD = #{const MUS_MP3_MAD}
musicTypeToConstant FLAC = #{const MUS_FLAC}
musicTypeToConstant MODPLUG = #{const MUS_MODPLUG}

constantToMusicType :: #{type Mix_MusicType} -> MusicType
constantToMusicType #{const MUS_NONE} = NONE
constantToMusicType #{const MUS_CMD} = CMD
constantToMusicType #{const MUS_WAV} = WAV
constantToMusicType #{const MUS_MOD} = MOD
constantToMusicType #{const MUS_MID} = MID
constantToMusicType #{const MUS_OGG} = OGG
constantToMusicType #{const MUS_MP3} = MP3
constantToMusicType #{const MUS_MP3_MAD} = MP3_MAD
constantToMusicType #{const MUS_FLAC} = FLAC
constantToMusicType #{const MUS_MODPLUG} = MODPLUG
constantToMusicType _ = error "invalid music type"

{- TODO
Mix_SetPostMix
Mix_HoolMusic
Mix_HookMusicFinished
Mix_GetMusicHookData
Mix_ChannelFinished
Mix_RegisterEffect
Mix_UnregisterEffect
Mix_UnregisterAllEffects
--}

foreign import ccall unsafe "Mix_SetPanning"
  mixSetPanning' :: #{type int} -> #{type Uint8} -> #{type Uint8} -> IO #{type int}

setPanning :: Channel -> Int -> Int -> IO ()
setPanning channel left right = do
  let channel' = fromIntegral channel
      left'    = fromIntegral left
      right'   = fromIntegral right
  ret <- mixSetPanning' channel' left' right'
  handleErrorI "setPanning" ret (==0) $ (const $ return ())

foreign import ccall unsafe "Mix_SetPosition"
  mixSetPosition' :: #{type int} -> #{type Sint16} -> #{type Uint8} -> IO #{type int}

setPosition :: Channel -> Int -> Int -> IO ()
setPosition channel angle distance = do
  let channel'  = fromIntegral channel
      angle'    = fromIntegral angle
      distance' = fromIntegral distance
  ret <- mixSetPosition' channel' angle' distance'
  handleErrorI "setPosition" ret (==0) $ (const $ return ())

foreign import ccall unsafe "Mix_SetDistance"
  mixSetDistance' :: #{type int} -> #{type Uint8} -> IO #{type int}

setDistance :: Channel -> Int -> IO ()
setDistance channel distance = do
  let channel'  = fromIntegral channel
      distance' = fromIntegral distance
  ret <- mixSetDistance' channel' distance'
  handleErrorI "setDistance" ret (==0) $ (const $ return ())

foreign import ccall unsafe "Mix_SetReverseStereo"
  mixSetReverseStereo' :: #{type int} -> #{type int} -> IO #{type int}

setReverseStereo :: Channel -> Bool -> IO ()
setReverseStereo channel doflip = do
  let channel' = fromIntegral channel
  ret <- mixSetReverseStereo' channel' (fromBool doflip)
  handleErrorI "setReverseStereo" ret (==0) $ (const $ return ())

foreign import ccall unsafe "Mix_ReserveChannels"
  mixReserveChannels' :: #{type int} -> IO #{type int}

reserveChannels :: Int -> IO ()
reserveChannels num = do
  _ <- mixReserveChannels' (fromIntegral num)
  return ()

foreign import ccall unsafe "Mix_GroupChannel"
  mixGroupChannel' :: #{type int} -> #{type int} -> IO #{type int}

groupChannel :: Channel -> Int -> IO Bool
groupChannel which tag =
  toBool <$> mixGroupChannel' (fromIntegral which) (fromIntegral tag)

foreign import ccall unsafe "Mix_GroupChannels"
  mixGroupChannels' :: #{type int} -> #{type int} -> #{type int} -> IO #{type int}

groupChannels :: Int -> Int -> Int -> IO Bool
groupChannels from to tag =
  let from' = fromIntegral from
      to'   = fromIntegral to
      tag'  = fromIntegral tag
  in toBool <$> mixGroupChannels' from' to' tag'

foreign import ccall unsafe "Mix_GroupAvailable"
  mixGroupAvailable' :: #{type int} -> IO #{type int}

groupAvailable :: Int -> IO Int
groupAvailable tag = fromIntegral <$> mixGroupAvailable' (fromIntegral tag)

foreign import ccall unsafe "Mix_GroupCount"
  mixGroupCount' :: #{type int} -> IO #{type int}

groupCount :: Int -> IO Int
groupCount tag = fromIntegral <$> mixGroupCount' (fromIntegral tag)

foreign import ccall unsafe "Mix_GroupOldest"
  mixGroupOldest' :: #{type int} -> IO #{type int}

groupOldest :: Int -> IO Int
groupOldest tag =
  fromIntegral <$> mixGroupOldest' (fromIntegral tag)

foreign import ccall unsafe "Mix_GroupNewer"
  mixGroupNewer' :: #{type int} -> IO #{type int}

groupNewer :: Int -> IO Int
groupNewer tag = fromIntegral <$> mixGroupNewer' (fromIntegral tag)

foreign import ccall unsafe "Mix_PlayChannelTimed"
  mixPlayChannelTimed' :: #{type int} -> Ptr ChunkStruct -> #{type int} -> #{type int} -> IO #{type int}

playChannelTimed :: Channel -> Chunk -> Int -> Int -> IO Int
playChannelTimed channel chunk loops ticks =
  let channel' = fromIntegral channel
      loops'   = fromIntegral loops
      ticks'   = fromIntegral ticks
  in withForeignPtr chunk $ \chunk' ->
       fromIntegral <$> mixPlayChannelTimed' channel' chunk' loops' ticks'

foreign import ccall unsafe "Mix_PlayMusic"
  mixPlayMusic' :: Ptr MusicStruct -> #{type int} -> IO #{type int}

playMusic :: Music -> Int -> IO ()
playMusic music loops =
  withForeignPtr music $ \music' -> do
    ret <- mixPlayMusic' music' (fromIntegral loops)
    handleErrorI "playMusic" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_FadeInMusic"
  mixFadeInMusic' :: Ptr MusicStruct -> #{type int} -> #{type int} -> IO #{type int}

fadeInMusic :: Music -> Int -> Int -> IO ()
fadeInMusic music loops ms =
  withForeignPtr music $ \music' -> do
    ret <- mixFadeInMusic' music' (fromIntegral loops) (fromIntegral ms)
    handleErrorI "fadeInMusic" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_FadeInMusicPos"
  mixFadeInMusicPos' :: Ptr MusicStruct -> #{type int} -> #{type int} -> #{type double} -> IO #{type int}

fadeInMusicPos :: Music -> Int -> Int -> Double -> IO ()
fadeInMusicPos music loops ms pos =
  withForeignPtr music $ \music' -> do
    ret <- mixFadeInMusicPos' music' (fromIntegral loops) (fromIntegral ms) pos
    handleErrorI "fadeInMusicPos" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_FadeInChannelTimed"
  mixFadeInChannelTimed' :: #{type int} -> Ptr ChunkStruct -> #{type int} -> #{type int} -> #{type int} -> IO #{type int}

fadeInChannelTimed :: Channel -> Chunk -> Int -> Int -> Int -> IO ()
fadeInChannelTimed channel chunk loops ms ticks =
  withForeignPtr chunk $ \chunk' -> do
    let channel' = fromIntegral channel
        loops'   = fromIntegral loops
        ms'      = fromIntegral ms
        ticks'   = fromIntegral ticks
    ret <- mixFadeInChannelTimed' channel' chunk' loops' ms' ticks'
    handleErrorI "fadeInChannelTimed" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_Volume"
  mixVolume' :: #{type int} -> #{type int} -> IO #{type int}

volume :: Channel -> Volume -> IO Volume
volume channel vol =
  cIntToVol <$> mixVolume' (fromIntegral channel) (volToCInt vol)

foreign import ccall unsafe "Mix_VolumeChunk"
  mixVolumeChunk' :: Ptr ChunkStruct -> #{type int} -> IO #{type int}

volumeChunk :: Chunk -> Volume -> IO Volume
volumeChunk chunk vol =
  withForeignPtr chunk $ \chunk' ->
    cIntToVol <$> mixVolumeChunk' chunk' (volToCInt vol)

foreign import ccall unsafe "Mix_VolumeMusic"
  mixVolumeMusic' :: Ptr MusicStruct -> #{type int} -> IO #{type int}

volumeMusic :: Music -> Volume -> IO Volume
volumeMusic music vol =
  withForeignPtr music $ \music' ->
    cIntToVol <$> mixVolumeMusic' music' (volToCInt vol)

volToCInt :: Volume -> #{type int}
volToCInt = fromIntegral . unwrapVolume

cIntToVol :: #{type int} -> Volume
cIntToVol = makeVolume . fromIntegral

foreign import ccall unsafe "Mix_HaltChannel"
  mixHaltChannel' :: #{type int} -> IO #{type int}

haltChannel :: Channel -> IO ()
haltChannel channel = mixHaltChannel' (fromIntegral channel) >> return ()

foreign import ccall unsafe "Mix_HaltGroup"
  mixHaltGroup' :: #{type int} -> IO #{type int}

haltGroup :: Int -> IO ()
haltGroup tag = mixHaltGroup' (fromIntegral tag) >> return ()

foreign import ccall unsafe "Mix_HaltMusic"
  mixHaltMusic' :: IO #{type int}

haltMusic :: IO ()
haltMusic = mixHaltMusic' >> return ()

foreign import ccall unsafe "Mix_ExpireChannel"
  mixExpireChannel' :: #{type int} -> #{type int} -> IO #{type int}

-- | returns number of channels set to expire
expireChannel :: Channel -> Int -> IO Int
expireChannel channel ticks =
  fromIntegral <$> mixExpireChannel' (fromIntegral channel) (fromIntegral ticks)

foreign import ccall unsafe "Mix_FadeOutChannel"
  mixFadeOutChannel' :: #{type int} -> #{type int} -> IO #{type int}

-- | returns the number of channels set to fade out
fadeOutChannel :: Channel -> Int -> IO Int
fadeOutChannel channel ms =
  fromIntegral <$> mixFadeOutChannel' (fromIntegral channel) (fromIntegral ms)

foreign import ccall unsafe "Mix_FadeOutGroup"
  mixFadeOutGroup' :: #{type int} -> #{type int} -> IO #{type int}

-- | returns the number of channels set to fade out
fadeOutGroup :: Int -> Int -> IO Int
fadeOutGroup tag ms =
  fromIntegral <$> mixFadeOutGroup' (fromIntegral tag) (fromIntegral ms)

foreign import ccall unsafe "Mix_FadeOutMusic"
  mixFadeOutMusic' :: #{type int} -> IO #{type int}

fadeOutMusic :: Int -> IO Bool
fadeOutMusic ms = toBool <$> mixFadeOutMusic' (fromIntegral ms)

foreign import ccall unsafe "Mix_FadingMusic"
  mixFadingMusic' :: IO #{type Mix_Fading}

constantToFading :: #{type Mix_Fading} -> Fading
constantToFading #{const MIX_NO_FADING} = NoFading
constantToFading #{const MIX_FADING_OUT} = FadingOut
constantToFading #{const MIX_FADING_IN} = FadingIn
constantToFading _ = error "invalid fading value"

fadingMusic :: IO Fading
fadingMusic = constantToFading <$> mixFadingMusic'

foreign import ccall unsafe "Mix_FadingChannel"
  mixFadingChannel' :: #{type int} -> IO #{type Mix_Fading}

fadingChannel :: Channel -> IO Fading
fadingChannel channel = constantToFading <$> mixFadingChannel' (fromIntegral channel)

foreign import ccall unsafe "Mix_Pause"
  mixPause' :: #{type int} -> IO ()

pause :: Channel -> IO ()
pause = mixPause' . fromIntegral

foreign import ccall unsafe "Mix_Resume"
  mixResume' :: #{type int} -> IO ()

resume :: Channel -> IO ()
resume = mixResume' . fromIntegral

foreign import ccall unsafe "Mix_Paused"
  mixPaused' :: #{type int} -> IO #{type int}

paused :: Channel -> IO Bool
paused channel = toBool <$> mixPaused' (fromIntegral channel)

foreign import ccall unsafe "Mix_PauseMusic"
  pauseMusic :: IO ()

foreign import ccall unsafe "Mix_ResumeMusic"
  resumeMusic :: IO ()

foreign import ccall unsafe "Mix_RewindMusic"
  rewindMusic :: IO ()

foreign import ccall unsafe "Mix_PausedMusic"
  mixPausedMusic' :: IO #{type int}

pausedMusic :: IO Bool
pausedMusic = toBool <$> mixPausedMusic'

foreign import ccall unsafe "Mix_SetMusicPosition"
  mixSetMusicPosition' :: #{type double} -> IO #{type int}

setMusicPosition :: Double -> IO ()
setMusicPosition pos = do
  ret <- mixSetMusicPosition' pos
  handleErrorI "setMusicPosition" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_Playing"
  mixPlaying' :: #{type int} -> IO #{type int}

playing :: Channel -> IO Bool
playing channel = toBool <$> mixPlaying' (fromIntegral channel)

foreign import ccall unsafe "Mix_PlayingMusic"
  mixPlayingMusic' :: IO #{type int}

playingMusic :: IO Bool
playingMusic = toBool <$> mixPlayingMusic'

foreign import ccall unsafe "Mix_SetMusicCMD"
  mixSetMusicCMD' :: CString -> IO #{type int}

setMusicCMD :: String -> IO ()
setMusicCMD cmd =
  withCString cmd $ \cmd' -> do
    ret <- mixSetMusicCMD' cmd'
    handleErrorI "setMusicCMD" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_SetSynchroValue"
  mixSetSynchroValue' :: #{type int} -> IO #{type int}

setSynchroValue :: Int -> IO ()
setSynchroValue value = do
  ret <- mixSetSynchroValue' (fromIntegral value)
  handleErrorI "setSynchroValue" ret (== -1) (const $ return ())

foreign import ccall unsafe "Mix_GetSynchroValue"
  mixGetSynchroValue' :: IO #{type int}

getSynchroValue :: IO Int
getSynchroValue = fromIntegral <$> mixGetSynchroValue'

foreign import ccall unsafe "Mix_SetSoundFonts"
  mixSetSoundFonts' :: CString -> IO Int

setSoundFonts :: String -> IO ()
setSoundFonts paths =
  withCString paths $ \paths' -> do
    ret <- mixSetSoundFonts' paths'
    handleErrorI "setSoundFonts" ret (== 0) (const $ return ())

foreign import ccall unsafe "Mix_GetSoundFonts"
  mixGetSoundFonts' :: IO CString

getSoundFonts :: IO String
getSoundFonts = mixGetSoundFonts' >>= peekCString

-- TODO Mix_EachSoundFont

foreign import ccall unsafe "Mix_GetChunk"
  mixGetChunk' :: #{type int} -> IO (Ptr ChunkStruct)

getChunk :: Channel -> IO Chunk
getChunk channel = mixGetChunk' (fromIntegral channel) >>= mkFinalizedChunk

foreign import ccall unsafe "Mix_CloseAudio"
  closeAudio :: IO ()

