module Lib.ImageMagick (
  thumbnailImage
  ) where

import ClassyPrelude
import Lib.ImageMagickFFI
import Graphics.ImageMagick.MagickWand.Utils

thumbnailImage pw w h = withException_ pw $! magickThumbnailImage pw (fromIntegral w) (fromIntegral h)
