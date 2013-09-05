{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

module Handler.Books where

import Import
import Yesod.Auth
import Data.Conduit (runResourceT, ($$))
import Data.Conduit.Binary (sinkLbs)
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

getBooksR :: Handler Html
getBooksR = do
  books <- runDB $ selectList [] [Asc BookId]
  defaultLayout $(widgetFile "books")

getAddBookR :: Handler Html
getAddBookR = do
  id <- requireAuthId
  ((res, form), enctype) <- runFormPost bookForm
  let reshowForm = defaultLayout $(widgetFile "bookform")
  case res of
    FormSuccess (title, maybeThumb, maybeFile, maybeUrl) -> do
      case maybeFile of
        Just file -> do 
          bid <- runDB $ insert $ Book title (fromString . unpack $ fileContentType file)
          liftIO $ saveThumbnail maybeThumb title $ showId bid
          content <- runResourceT $ fileSource file $$ sinkLbs
          liftIO $ saveFile (repack content) (showId bid)
          redirect $ BookR bid
        Nothing -> 
          case maybeUrl of
            Just url -> do
              res <- liftIO $ downloadFile url
              case res of
                Right (contentType, body) -> do
                  bid <- runDB $ insert $ Book title (fromString contentType)
                  liftIO $ saveThumbnail maybeThumb title $ showId bid
                  liftIO $ saveFile body $ showId bid
                  redirect $ BookR bid
                Left err -> do
                  setMessage $ toHtml err
                  reshowForm
            Nothing -> do
              setMessage "Upload file or provide a link."
              reshowForm
    _ -> do
      setMessage "Fill all required fields."
      reshowForm

postAddBookR :: Handler Html
postAddBookR = getAddBookR

getBookR :: BookId -> Handler Html
getBookR bid = do
  Book title (show -> contentType :: Text) <- runDB $ get404 bid
  let imageSrc = "/static/img/thumbs/" ++ showId bid :: Text
      fileUrl = "/download/" ++ showId bid :: Text
  defaultLayout $(widgetFile "book")
      
getDownloadR :: BookId -> Handler Html
getDownloadR bid = do
  Book title contentType <- runDB $ get404 bid
  let file = ResponseFile ok200 
        [("Content-Disposition", "filename=" ++ encodeUtf8 title ), (hContentType, contentType)] 
        ("static/books/" ++ showId bid) Nothing
  sendWaiResponse file

bookForm :: Form (Text,Maybe FileInfo,Maybe FileInfo,Maybe Text)
bookForm = renderDivs $ (,,,)
    <$> areq textField "Title" Nothing
    <*> fileAFormOpt "Thumbnail image"
    <*> fileAFormOpt "Upload file"
    <*> aopt textField "File URL" Nothing
