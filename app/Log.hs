module Log where

import Text.Printf
import System.IO

printLog :: String -> IO ()
printLog str = hPutStrLn stderr (printf "[retrohack] %s\n" str)
