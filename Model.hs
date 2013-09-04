module Model where

import ClassyPrelude
import ClassyPrelude.Classes
import Yesod
import Database.Persist.Quasi
import Data.Conduit
import Data.Conduit.Binary
import Graphics.ImageMagick.MagickWand
import Network.Curl

showPersist (PersistInt64 i) = show i
showPersist _ = undefined 

showId = showPersist . unKey

saveThumbnail :: FileInfo -> FilePath ->  IO ()
saveThumbnail file name = withMagickWandGenesis $ do
  (_,w) <- magickWand
  blob <- runResourceT $ fileSource file $$ sinkLbs
  readImageBlob w $ repack blob
  extentImage w 120 150 0 0
  writeImage w $ Just $ "static/img/covers" </> name

saveFile :: ByteString -> FilePath ->  IO ()
saveFile body name = writeFile (repack $  "static/books" </> name) body

downloadFile :: Text -> IO (Either String (String,String))
downloadFile url = do
  resp <- curlGetResponse (unpack url) []
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
