{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib.ImageMagickFFI where

import           Prelude
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types

#include "/usr/include/ImageMagick/wand/MagickWand.h"

foreign import ccall "MagickThumbnailImage" magickThumbnailImage
  :: Ptr MagickWand
  -> CSize                        -- ^ the number of columns
  -> CSize                        -- ^ the number of rows
  -> IO MagickBooleanType
