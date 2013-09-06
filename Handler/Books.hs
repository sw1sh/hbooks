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
getAddBookR = modifyBookR Nothing

postAddBookR :: Handler Html
postAddBookR = getAddBookR

getEditBookR :: BookId -> Handler Html
getEditBookR bid = modifyBookR $ Just bid

postEditBookR :: BookId -> Handler Html
postEditBookR = getEditBookR

getDeleteBookR :: BookId -> Handler Html
getDeleteBookR bid = do
  requireAuth
  runDB $ delete bid
  redirect BooksR

modifyBook maybeBid book =
  case maybeBid of
    Just bid -> do
      runDB $ replace bid book
      return bid
    Nothing -> do
      runDB $ insert book

modifyBookR maybeBid = do
  requireAuth
  alreadyExpired
  maybeBook <- maybe (return Nothing) (runDB . get) maybeBid 
  ((res, form), enctype) <- runFormPost $ bookForm maybeBook
  let reshowForm = defaultLayout $(widgetFile "bookform")
  case res of
    FormSuccess (title, maybeThumb, maybeFile, maybeUrl) -> do
      let finishAction bid = do
            liftIO $ saveThumbnail maybeThumb title $ showId bid
            redirect $ BookR bid
      case maybeFile of
        Just file -> do 
          bid <- modifyBook maybeBid $ Book title (fromString . unpack $ fileContentType file)
          content <- runResourceT $ fileSource file $$ sinkLbs
          liftIO $ saveFile (repack content) (showId bid)
          finishAction bid
        Nothing -> 
          case maybeUrl of
            Just url -> do
              res <- liftIO $ downloadFile url
              case res of
                Right (contentType, body) -> do
                  bid <- modifyBook maybeBid $ Book title (fromString contentType)
                  liftIO $ saveFile body $ showId bid
                  finishAction bid
                Left err -> do
                  setMessage $ toHtml err
                  reshowForm
            Nothing ->
              case maybeBid of
                Just bid -> do
                  runDB $ update bid [BookTitle =. title]
                  finishAction bid
                Nothing -> do
                  setMessage "Upload file or provide a link."
                  reshowForm
    _ -> do
      setMessage "Fill all required fields."
      reshowForm

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

bookForm :: Maybe Book -> Form (Text, Maybe FileInfo, Maybe FileInfo, Maybe Text)
bookForm maybeBook = renderDivs $ (,,,)
    <$> areq textField "Title" (bookTitle <$> maybeBook)
    <*> fileAFormOpt "Thumbnail image"
    <*> fileAFormOpt "Upload file"
    <*> aopt textField "File URL" Nothing
