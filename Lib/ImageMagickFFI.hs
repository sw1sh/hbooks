{-# LINE 1 "ImageMagickFFI.hsc" #-}
{-# LANGUAGE CPP                      #-}
{-# LINE 2 "ImageMagickFFI.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Lib.ImageMagickFFI where

import           Prelude
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Graphics.ImageMagick.MagickCore.Types
import           Graphics.ImageMagick.MagickWand.FFI.Types


{-# LINE 15 "ImageMagickFFI.hsc" #-}

foreign import ccall "MagickThumbnailImage" magickThumbnailImage
  :: Ptr MagickWand
  -> CSize                        -- ^ the number of columns
  -> CSize                        -- ^ the number of rows
  -> IO MagickBooleanType
