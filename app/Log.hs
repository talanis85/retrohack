module Log where

import Text.Printf

printLog :: String -> IO ()
printLog str = printf "[retrohack] %s\n" str
