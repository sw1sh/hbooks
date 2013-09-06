module Model where

import ClassyPrelude
import Yesod
import Database.Persist.Quasi
import Data.Conduit
import Data.Conduit.Binary
import Graphics.ImageMagick.MagickWand
import Network.Curl
import Lib.ImageMagick

showPersist (PersistInt64 i) = show i
showPersist _ = undefined 

showId = showPersist . unKey

splitWords = concat . intersperse "\n" . words

saveThumbnail :: Maybe FileInfo -> Text -> FilePath ->  IO ()
saveThumbnail (Just fi) _ name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  blob <- repack <$> (runResourceT $ fileSource fi $$ sinkLbs)
  readImageBlob w blob
  thumbnailImage w 120 150
  writeImage w $ Just $ "static/img/thumbs" </> name
saveThumbnail Nothing title name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  (_,dw) <- drawingWand
  pw <- pixelWand

  pw `setColor` "none"
  -- Create a new transparent image
  newImage w 120 150 pw
  -- Set up a 9 point white font
  pw `setColor` "white"
  dw `setFillColor` pw
  dw `setFont` "VerdanaBI"
  dw `setFontSize` 10
  -- Add a black outline to the text
  pw `setColor` "black"
  dw `setStrokeColor` pw
  -- Turn antialias on - not sure this makes a difference
  dw `setTextAntialias` True
  -- Now draw the text
  drawAnnotation dw 0 20 $ splitWords title
  -- Draw the image on to the magick_wand
  drawImage w dw
  setImageFormat w "png"
  writeImage w $ Just $ "static/img/thumbs" </> name
 
saveFile :: ByteString -> FilePath ->  IO ()
saveFile body name = writeFile (repack $  "static/books" </> name) body

downloadFile :: Text -> IO (Either String (String, ByteString))
downloadFile url = do
  resp <- curlGetResponse_ (unpack url) [CurlUserAgent "curl"]
    :: IO (CurlResponse_ [(String, String)] ByteString)
  IDouble size <- respGetInfo resp SizeDownload
  IString contentType <- respGetInfo resp ContentType
  case respCurlCode resp of
    CurlOK -> return $ Right (contentType, respBody resp)
    err -> return $ Left $ show err

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
