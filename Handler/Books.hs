{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

module Handler.Books where

import Import
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
  ((res, form), enctype) <- runFormPost bookForm
  let reshowForm = defaultLayout $(widgetFile "bookform")
  case res of
    FormSuccess (title, cover, maybeFile, maybeUrl) -> do
      case maybeFile of
        Just file -> do 
          cid <- runDB $ insert $ Cover $ fileName cover
          liftIO $ saveThumbnail cover $ showId cid
          bid <- runDB $ insert $ Book title cid (fromString . unpack $ fileContentType file)
          content <- runResourceT $ fileSource file $$ sinkLbs
          liftIO $ saveFile (repack content) (showId bid)
          redirect $ BookR bid
        Nothing -> 
          case maybeUrl of
            Just url -> do
              res <- liftIO $ downloadFile url
              case res of
                Right (contentType, body) -> do
                  cid <- runDB $ insert $ Cover $ fileName cover
                  liftIO $ saveThumbnail cover $ showId cid
                  bid <- runDB $ insert $ Book title cid (fromString contentType)
                  liftIO $ saveFile (fromString body) $ showId bid
                  redirect $ BookR bid
                Left err -> reshowForm
            Nothing -> reshowForm
    _ -> reshowForm

postAddBookR :: Handler Html
postAddBookR = getAddBookR

getBookR :: BookId -> Handler Html
getBookR bid = do
  Book title cid (show -> contentType :: Text) <- runDB $ get404 bid
  let imageSrc = "/static/img/covers/" ++ showId cid :: Text
      fileUrl = "/download/" ++ showId bid :: Text
  defaultLayout $(widgetFile "book")
      
getDownloadR :: BookId -> Handler Html
getDownloadR bid = do
  Book title _ contentType <- runDB $ get404 bid
  let file = ResponseFile ok200 
        [("Content-Disposition", "filename=" ++ encodeUtf8 title ), (hContentType, contentType)] 
        ("static/books/" ++ showId bid) Nothing
  sendWaiResponse file

bookForm :: Form (Text,FileInfo,Maybe FileInfo,Maybe Text)
bookForm = renderDivs $ (,,,)
    <$> areq textField "Title" Nothing
    <*> fileAFormReq "Cover image"
    <*> fileAFormOpt "Upload file"
    <*> aopt textField "File URL" Nothing
