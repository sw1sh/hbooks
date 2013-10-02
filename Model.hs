{-# LANGUAGE RecordWildCards #-}

module Model (
  module Lib.MyPersist,
  module Model
  ) where

import ClassyPrelude
import Prelude (read)
import Yesod
import Database.Persist.Quasi
import Data.Conduit
import Data.Conduit.Binary (sinkLbs)
import Graphics.ImageMagick.MagickWand
import Network.Curl
import Numeric (showHex)
import Lib.ImageMagick
import Lib.MyPersist
import Lib.S3

showPersist (PersistInt64 i) = fromString $ show i
showPersist _ = undefined 

showId = showPersist . unKey

splitWords = concat . intersperse "\n" . words

getFileExt = reverse . takeWhile (/='.') . reverse

saveThumbnail :: Maybe (Text, ByteString) -> Text -> FilePath ->  IO ()
saveThumbnail (Just (contentType, blob)) _ name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  pw <- pixelWand
  readImageBlob w blob
  setColor pw "none"
  setImageBackgroundColor w pw
  width <- getImageWidth w
  height <- getImageHeight w
  let s = max width height
      sx = s - width
      sy = s - height
  extentImage w s s (-sx`div`2) (-sy`div`2)
  resizeImage w 120 150 lanczosFilter 1
  blob <- getImageBlob w
  lift $ uploadToS3 ("img" </> name) contentType [] blob

saveThumbnail Nothing title name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  (_,dw) <- drawingWand
  pw <- pixelWand

  pw `setColor` "none"
  -- Create a new transparent image
  newImage w 120 150 pw
  -- Set up a 9 point white font
  pw `setColor` "black"
  dw `setFillColor` pw
  dw `setFont` "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSans.ttf"
  dw `setFontSize` 10
  -- Add a black outline to the text
--  pw `setColor` "black"
--  dw `setStrokeColor` pw
  -- Turn antialias on - not sure this makes a difference
  dw `setTextAntialias` True
  -- Now draw the text
  drawAnnotation dw 0 20 $ splitWords title
  -- Draw the image on to the magick_wand
  drawImage w dw
  setImageFormat w "png"
  blob <- getImageBlob w
  lift $ uploadToS3 ("img" </> name) "image/png" [] blob
 
saveFile :: FilePath -> Text -> Text -> ByteString -> IO ()
saveFile name contentType filename blob = uploadToS3 ("files" </> name) contentType 
  [("Content-Disposition", "filename*=UTF-8''" ++ escapeUtf filename)] blob

escapeUtf = concat . map (\w -> fromString $ "%" ++ showHex w "") . unpack . encodeUtf8

downloadFile :: Text -> IO (Either String (String, String, ByteString))
downloadFile url = do
  resp <- curlGetResponse_ (unpack url) [CurlUserAgent "Mozilla/4.0", CurlFollowLocation True, CurlAutoReferer True]
    :: IO (CurlResponse_ [(String, String)] ByteString)
  IDouble size <- respGetInfo resp SizeDownload
  IString contentType <- respGetInfo resp ContentType
  IString url <- respGetInfo resp EffectiveUrl
  case respCurlCode resp of
    CurlOK -> return $ Right (contentType, getFileExt url, respBody resp)
    err -> return $ Left $ show err

instance ToJSON BookType where
  toJSON = toJSON . tshow

instance ToJSON BookCategory where
  toJSON = toJSON . tshow

instance FromJSON BookType where
  parseJSON (String f) = return $ read $ unpack f

instance FromJSON BookCategory where
  parseJSON (String f) = return $ read $ unpack f
  
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

showTags :: [Tag] -> Text
showTags = concat . intersperse " " . map tagName

toTags :: Maybe Text -> [Tag]
toTags (Just t) = map Tag $ words t
toTags Nothing = []

