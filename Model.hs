module Model where

import ClassyPrelude
import Yesod
import Database.Persist.Quasi
import Data.Conduit
import Data.Conduit.Binary
import Graphics.ImageMagick.MagickWand

toThumbnail :: FileInfo -> IO ()
toThumbnail f = withMagickWandGenesis $ do
  (_,w) <- magickWand
  blob <- runResourceT $ fileSource f $$ sinkLbs
  readImageBlob w $ repack blob
  resizeImage w 150 120 lanczosFilter 1
  writeImage w $ Just $ "static/img/" </> repack (fileName f)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
