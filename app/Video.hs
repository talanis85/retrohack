{-# LANGUAGE TemplateHaskell #-}
module Video
  ( Video
  , initVideo
  , configureVideo
  , deconfigureVideo
  , videoRefresh
  , videoRender
  , videoSetPixelFormat
  , windowShouldClose
  , videoInputPoll
  , videoInputState
  , defaultInputMappings
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Data.IORef
import qualified Data.Map as Map
import Foreign
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GL3W as GL3W
import Text.Printf

import Log
import Libretro

data VideoConfig = VideoConfig
  { _videoConfigPixelFormat :: RetroPixelFormat
  }

data VideoState = VideoState
  { _videoStateTexture :: TextureObject
  , _videoStateWindow :: GLFW.Window
  , _videoStatePitch :: Word32
  , _videoStateClip :: (Word32, Word32)
  , _videoStateTextureSize :: (Word32, Word32)
  , _videoStateTextureCoords :: Ptr (TexCoord2 GLfloat)
  , _videoStateVertex :: Ptr (Vertex2 GLfloat)
  , _videoStateInput :: Map.Map (Word32, RetroDeviceIdJoypad) Int16
  }

makeLenses ''VideoConfig
makeLenses ''VideoState

type Video = IORef (VideoConfig, Maybe VideoState)

initVideo :: IO Video
initVideo = do
  glfwInitSuccess <- GLFW.init
  when (not glfwInitSuccess) $ ioError $ userError "Could not initialize GLFW"

  let config = VideoConfig
        { _videoConfigPixelFormat = retroPixelFormat0RGB1555
        }
      state = Nothing
  newIORef (config, state)

windowShouldClose :: Video -> IO Bool
windowShouldClose vsRef = liftIO $ readIORef vsRef >>= windowShouldClose'
  where
    windowShouldClose' (config, Nothing) = return False
    windowShouldClose' (config, Just vs) = GLFW.windowShouldClose (vs ^. videoStateWindow)

configureVideo :: Video -> RetroGameGeometry -> IO ()
configureVideo vsRef geometry = liftIO $ readIORef vsRef >>= configureVideo'
  where
    configureVideo' (config, Just _) = error "'configureVideo' called twice"
    configureVideo' (config, Nothing) = do
      let (width, height) = resizeToAspect geometry & both *~ 1

      window <- createWindow width height
      GLFW.setWindowSize window (fromIntegral width) (fromIntegral height)

      let textureWidth = retroGameGeometryMaxWidth geometry
          textureHeight = retroGameGeometryMaxHeight geometry

      let pixelFormat = getPixelFormat (config ^. videoConfigPixelFormat)
          dataType = getDataType (config ^. videoConfigPixelFormat)
          bpp = getBpp (config ^. videoConfigPixelFormat)

      texture <- genObjectName
      textureBinding Texture2D $= Just texture
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      texImage2D
        Texture2D
        NoProxy
        0
        RGBA8
        (TextureSize2D (fromIntegral textureWidth) (fromIntegral textureHeight))
        0
        (PixelData pixelFormat dataType nullPtr)
      textureBinding Texture2D $= Nothing

      coordsPointer <- newArray
        [ TexCoord2 0.0 1.0
        , TexCoord2 0.0 0.0
        , TexCoord2 1.0 1.0
        , TexCoord2 1.0 0.0
        ]

      vertexPointer <- newArray
        [ Vertex2 (-1.0) (-1.0)
        , Vertex2 (-1.0) 1.0
        , Vertex2 1.0 (-1.0)
        , Vertex2 1.0 1.0
        ]

      let videoState = VideoState
            { _videoStateTexture = texture
            , _videoStateWindow = window
            , _videoStatePitch = retroGameGeometryBaseWidth geometry * bpp
            , _videoStateClip = (retroGameGeometryBaseWidth geometry, retroGameGeometryBaseHeight geometry)
            , _videoStateTextureSize = (textureWidth, textureHeight)
            , _videoStateTextureCoords = coordsPointer
            , _videoStateVertex = vertexPointer
            , _videoStateInput = Map.empty
            }

      printLog $ "Video configuration:"
      printLog $ printf "  pixelFormat=%s" (show pixelFormat)
      printLog $ printf "  dataType=%s" (show dataType)
      printLog $ printf "  bpp=%s" (show bpp)
      printLog $ printf "  pitch=%s" (show (videoState ^. videoStatePitch))
      printLog $ printf "  clip=%s" (show (videoState ^. videoStateClip))
      printLog $ printf "  textureSize=%s" (show (videoState ^. videoStateTextureSize))

      writeIORef vsRef (config, Just videoState)

      refreshVertexData vsRef

deconfigureVideo :: Video -> IO ()
deconfigureVideo vsRef = liftIO $ readIORef vsRef >>= deconfigureVideo'
  where
    deconfigureVideo' (config, Nothing) = return ()
    deconfigureVideo' (config, Just vs) = do
      deleteObjectName (vs ^. videoStateTexture)
      free (vs ^. videoStateVertex)
      free (vs ^. videoStateTextureCoords)
      GLFW.destroyWindow (vs ^. videoStateWindow)
      writeIORef vsRef (config, Nothing)

resizeToAspect :: RetroGameGeometry -> (Word32, Word32)
resizeToAspect geometry = (dw, dh)
  where
    w = retroGameGeometryBaseWidth geometry
    h = retroGameGeometryBaseHeight geometry
    ratio = retroGameGeometryAspectRatio geometry
    ratio' = if ratio <= 0 then fromIntegral w / fromIntegral h else ratio
    (dw, dh) = if fromIntegral w / fromIntegral h < 1
                  then (floor $ fromIntegral h * ratio', h)
                  else (w, floor $ fromIntegral w / ratio')

refreshVertexData :: Video -> IO ()
refreshVertexData vsRef = liftIO $ readIORef vsRef >>= refreshVertexData'
  where
    refreshVertexData' (config, Nothing) = return ()
    refreshVertexData' (config, Just vs) = do
      let (clipw, cliph) = vs ^. videoStateClip
          (texw, texh) = vs ^. videoStateTextureSize

      let cx = fromIntegral clipw / fromIntegral texw
          cy = fromIntegral cliph / fromIntegral texh

      let peekpokeElemOff p n f = do
            x <- peekElemOff p n
            pokeElemOff p n (f x)

      peekpokeElemOff (vs ^. videoStateTextureCoords) 0 (\(TexCoord2 x y) -> TexCoord2 x cy)
      peekpokeElemOff (vs ^. videoStateTextureCoords) 2 (\(TexCoord2 x y) -> TexCoord2 cx cy)
      peekpokeElemOff (vs ^. videoStateTextureCoords) 3 (\(TexCoord2 x y) -> TexCoord2 cx y)

createWindow :: Word32 -> Word32 -> IO GLFW.Window
createWindow width height = do
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 2)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 1)
  GLFW.windowHint (GLFW.WindowHint'Resizable True)

  Just window <- GLFW.createWindow (fromIntegral width) (fromIntegral height) "retrohack" Nothing Nothing

  GLFW.setFramebufferSizeCallback window (Just resizeCallback)
  GLFW.makeContextCurrent (Just window)

  GL3W.gl3wInit

  GLFW.swapInterval 1

  texture Texture2D $= Enabled

  version <- get shadingLanguageVersion
  printLog $ printf "GLSL Version: %s" version

  resizeCallback window (fromIntegral width) (fromIntegral height)
  return window

resizeCallback :: GLFW.Window -> Int -> Int -> IO ()
resizeCallback window w h = do
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

videoRefresh :: Video -> PixelPtr -> Word32 -> Word32 -> Word32 -> RetroM ()
videoRefresh vsRef dat width height pitch = liftIO $ readIORef vsRef >>= videoRefresh'
  where
    videoRefresh' (config, Nothing) = return ()
    videoRefresh' (config, Just vs) = do
      let (clipw, cliph) = vs ^. videoStateClip

      -- printLog $ printf "width=%s height=%s pitch=%s" (show width) (show height) (show pitch)

      when (clipw /= width || cliph /= height) $ do
        modifyIORef vsRef $ _2 . traversed . videoStateClip .~ (width, height)
        refreshVertexData vsRef

      textureBinding Texture2D $= Just (vs ^. videoStateTexture)
      
      let pixelFormat = getPixelFormat (config ^. videoConfigPixelFormat)
          dataType = getDataType (config ^. videoConfigPixelFormat)
          bpp = getBpp (config ^. videoConfigPixelFormat)

      when (pitch /= vs ^. videoStatePitch) $ do
        rowLength Unpack $= fromIntegral (pitch `div` bpp)
        modifyIORef vsRef $ _2 . traversed . videoStatePitch .~ pitch

      when (dat == nullPtr) $
        printLog "videoRefresh called with NULL"

      when (dat /= nullPtr) $ texSubImage2D
        Texture2D
        0
        (TexturePosition2D 0 0)
        (TextureSize2D (fromIntegral width) (fromIntegral height))
        (PixelData pixelFormat dataType dat)

videoRender :: Video -> IO ()
videoRender vsRef = liftIO $ readIORef vsRef >>= videoRender'
  where
    videoRender' (config, Nothing) = return ()
    videoRender' (config, Just vs) = do
      GLFW.pollEvents
      clear [ColorBuffer]

      textureBinding Texture2D $= Just (vs ^. videoStateTexture)
      clientState VertexArray $= Enabled
      clientState TextureCoordArray $= Enabled
      arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 0 (vs ^. videoStateVertex)
      arrayPointer TextureCoordArray $= VertexArrayDescriptor 2 Float 0 (vs ^. videoStateTextureCoords)
      drawArrays TriangleStrip 0 4

      GLFW.swapBuffers (vs ^. videoStateWindow)

videoSetPixelFormat :: Video -> RetroPixelFormat -> RetroM Bool
videoSetPixelFormat vsRef format = liftIO $ readIORef vsRef >>= videoSetPixelFormat'
  where
    videoSetPixelFormat' (config, Just _) = return False
    videoSetPixelFormat' (config, Nothing)
      | format == retroPixelFormat0RGB1555 = do
          modifyIORef vsRef $ _1 . videoConfigPixelFormat .~ retroPixelFormat0RGB1555
          printLog "Pixel format: 0RGB1555, native endian"
          return True
      | format == retroPixelFormatXRGB8888 = do
          modifyIORef vsRef $ _1 . videoConfigPixelFormat .~ retroPixelFormatXRGB8888
          printLog "Pixel format: XRGB8888, native endian"
          return True
      | format == retroPixelFormatRGB565 = do
          modifyIORef vsRef $ _1 . videoConfigPixelFormat .~ retroPixelFormatRGB565
          printLog "Pixel format: RGB565, native endian"
          return True
      | otherwise = do
          printLog "Pixel format: UNKNOWN"
          return False

getPixelFormat :: RetroPixelFormat -> PixelFormat
getPixelFormat format
  | format == retroPixelFormat0RGB1555 = BGRA
  | format == retroPixelFormatXRGB8888 = BGRA
  | format == retroPixelFormatRGB565 = RGB
  | otherwise = error "Invalid pixel format"

getDataType :: RetroPixelFormat -> DataType
getDataType format
  | format == retroPixelFormat0RGB1555 = UnsignedShort5551
  | format == retroPixelFormatXRGB8888 = UnsignedInt8888Rev
  | format == retroPixelFormatRGB565 = UnsignedShort565
  | otherwise = error "Invalid pixel format"

getBpp :: RetroPixelFormat -> Word32
getBpp format
  | format == retroPixelFormat0RGB1555 = 2
  | format == retroPixelFormatXRGB8888 = 4
  | format == retroPixelFormatRGB565 = 2
  | otherwise = error "Invalid pixel format"

data GLFWControl = KeyControl GLFW.Key | GamepadButtonControl GLFW.Joystick GLFW.GamepadButton

defaultInputMappings :: [(GLFWControl, Word32, RetroDeviceIdJoypad)]
defaultInputMappings =
  -- The Xbox controller is so wrong here!
  [ (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'A, 0, retroDeviceIdJoypadB)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'B, 0, retroDeviceIdJoypadA)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'X, 0, retroDeviceIdJoypadY)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'Y, 0, retroDeviceIdJoypadX)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'Start, 0, retroDeviceIdJoypadStart)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'Back, 0, retroDeviceIdJoypadSelect)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'LeftBumper, 0, retroDeviceIdJoypadL)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'RightBumper, 0, retroDeviceIdJoypadR)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'DpadUp, 0, retroDeviceIdJoypadUp)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'DpadDown, 0, retroDeviceIdJoypadDown)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'DpadLeft, 0, retroDeviceIdJoypadLeft)
  , (GamepadButtonControl GLFW.Joystick'1 GLFW.GamepadButton'DpadRight, 0, retroDeviceIdJoypadRight)
  ]

videoInputPoll :: [(GLFWControl, Word32, RetroDeviceIdJoypad)] -> Video -> RetroM ()
videoInputPoll binds vsRef = liftIO $ readIORef vsRef >>= videoInputPoll'
  where
    videoInputPoll' (config, Nothing) = return ()
    videoInputPoll' (config, Just vs) = forM_ binds $ \(ctl, port, devid) -> do
      value <- pollControl (vs ^. videoStateWindow) ctl
      modifyIORef vsRef $ _2 . traversed . videoStateInput %~ Map.insert (port, devid) value

pollControl :: GLFW.Window -> GLFWControl -> IO Int16
pollControl window (KeyControl x) = do
  s <- GLFW.getKey window x
  if s == GLFW.KeyState'Released then return 0 else return 1
pollControl window (GamepadButtonControl joystick button) = do
  s <- GLFW.getGamepadState joystick
  case s of
    Nothing -> return 0
    Just s' -> if GLFW.getButtonState s' button == GLFW.GamepadButtonState'Released
      then return 0
      else return 1

videoInputState :: Video -> Word32 -> RetroDevice -> Word32 -> Word32 -> RetroM Int16
videoInputState vsRef port device index id = liftIO $ readIORef vsRef >>= videoInputState'
  where
    videoInputState' (config, Nothing) = return 0
    videoInputState' (config, Just vs) =
      if device /= retroDeviceJoypad
        then return 0
        else case Map.lookup (port, fromIntegral id) (vs ^. videoStateInput) of
          Nothing -> return 0
          Just v -> return v
