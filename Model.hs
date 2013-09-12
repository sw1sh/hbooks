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
import Lib.ImageMagick
import Lib.MyPersist
import Lib.S3

showPersist (PersistInt64 i) = show i
showPersist _ = undefined 

showId = showPersist . unKey

splitWords = concat . intersperse "\n" . words

getFileExt = reverse . takeWhile (/='.') . reverse

saveThumbnail :: Maybe FileInfo -> Text -> FilePath ->  IO ()
saveThumbnail (Just fi) _ name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  blob <- repack <$> (runResourceT $ fileSource fi $$ sinkLbs)
  readImageBlob w blob
  thumbnailImage w 120 150
--  writeImage w $ Just $ "static/img/thumbs" </> name
  blob <- getImageBlob w
  lift $ uploadToS3 ("img" </> name) (fileContentType fi) [] blob
saveThumbnail Nothing title name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  (_,dw) <- drawingWand
  pw <- pixelWand

  pw `setColor` "white"
  -- Create a new transparent image
  newImage w 120 150 pw
  -- Set up a 9 point white font
  pw `setColor` "black"
  dw `setFillColor` pw
  dw `setFont` "Arial"
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
  [("Content-Disposition", "filename=\"" ++ filename ++ "\"")] blob

downloadFile :: Text -> IO (Either String (String, String, ByteString))
downloadFile url = do
  resp <- curlGetResponse_ (unpack url) [CurlUserAgent "curl"]
    :: IO (CurlResponse_ [(String, String)] ByteString)
  IDouble size <- respGetInfo resp SizeDownload
  IString contentType <- respGetInfo resp ContentType
  IString url <- respGetInfo resp EffectiveUrl
  case respCurlCode resp of
    CurlOK -> return $ Right (contentType, getFileExt url, respBody resp)
    err -> return $ Left $ show err

instance ToJSON BookType where
  toJSON bt = toJSON (show bt :: Text)

instance ToJSON BookCategory where
  toJSON bt = toJSON (show bt :: Text)

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

