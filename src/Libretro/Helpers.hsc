{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Libretro.Helpers where

import Foreign

import Libretro.Foreign

#include "helpers.h"

foreign import ccall "default_retro_log_printf" default_retro_log_printf
  :: IO (FunPtr RetroLogPrintfT)
