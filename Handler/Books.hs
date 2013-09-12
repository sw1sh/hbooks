{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, RecordWildCards #-}

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
  tags <- runDB $ selectList [] [Asc TagId]
  defaultLayout $(widgetFile "books")

getAddBookR :: Handler Html
getAddBookR = modifyBookR Nothing

postAddBookR :: Handler Html
postAddBookR = getAddBookR

getEditBookR :: BookId -> Handler Html
getEditBookR bid = modifyBookR $ Just bid

postEditBookR :: BookId -> Handler Html
postEditBookR = getEditBookR

deleteBookR :: BookId -> Handler Html
deleteBookR bid = do
  uid <- requireAuthId
  book <- runDB $ get404 bid
  let ownerId = bookOwner book
  if uid == ownerId then do
    runDB $ delete bid 
    redirect BooksR
    else
      error "You're not allowed to delete this!"

modifyBook uid maybeBid book =
  case maybeBid of
    Just bid ->
      if uid == bookOwner book then do
        runDB $ replace bid book
        return bid
      else
        error "You're not allowed to edit this!"
    Nothing -> do
      runDB $ insert book

modifyBookR maybeBid = do
  uid <- requireAuthId
  alreadyExpired
  maybeBook <- maybe (return Nothing) (runDB . get) maybeBid
  tags <- getTags $ bookTags <$> maybeBook
  ((res, form), enctype) <- runFormPost $ bookForm maybeBook tags
  let reshowForm = defaultLayout $(widgetFile "bookform")
  case res of
    FormSuccess ( title, 
                  author, 
                  typ, 
                  category,
                  toTags -> tags,
                  maybeThumb, 
                  maybeFile, 
                  maybeUrl, 
                  isDownload) -> do
      
      maybeTags <- mapM (runDB . getByValue) tags
      let tagIds = map (\(Entity id _) -> id) $ catMaybes maybeTags
          finishAction bid = do
            liftIO $ saveThumbnail maybeThumb title $ showId bid
            redirect $ BookR bid
      case maybeFile of
        Just file -> do    
          let ext = getFileExt $ fileName file
          bid <- modifyBook uid maybeBid 
            $ Book { bookOwner = uid,
                     bookTitle = title,
                     bookAuthor = author,
                     bookContentType = fromString . unpack $ fileContentType file,
                     bookContentExtension = ext,
                     bookExternalLink = Nothing,
                     bookType = typ,
                     bookCategory = category,
                     bookTags = tagIds }
          content <- runResourceT $ fileSource file $$ sinkLbs
          liftIO $ saveFile (showId bid) (fileContentType file) (title ++ "." ++ ext) (repack content)
          finishAction bid
        Nothing -> 
          case (maybeUrl, isDownload) of
            (Just url, True) -> do
              res <- liftIO $ downloadFile url
              case res of
                Right (contentType, ext, body) -> do
                  bid <- modifyBook uid maybeBid 
                    $ Book { bookOwner = uid,
                             bookTitle = title,
                             bookAuthor = author,
                             bookContentType = fromString contentType,
                             bookContentExtension = fromString ext, 
                             bookExternalLink = Nothing,
                             bookType = typ,
                             bookCategory = category,
                             bookTags = tagIds }
                  liftIO $ saveFile (showId bid) (fromString contentType) (title ++ "." ++ fromString ext) body
                  finishAction bid
                Left err -> do
                  setMessage $ toHtml err
                  reshowForm
            (Just url, False) -> do
              bid <- modifyBook uid maybeBid 
                $ Book { bookOwner = uid,
                         bookTitle = title,
                         bookAuthor = author,
                         bookContentType = "link",
                         bookContentExtension = "", 
                         bookExternalLink = Just url,
                         bookType = typ,
                         bookCategory = category,
                         bookTags = tagIds }
              finishAction bid
            (Nothing, _) ->
              case maybeBid of
                Just bid -> do
                  runDB $ update bid [ BookTitle =. title, 
                                       BookType =. typ, 
                                       BookCategory =. category, 
                                       BookTags =. tagIds ]
                  finishAction bid
                Nothing -> do
                  setMessageI MsgUploadOrProvideUrl
                  reshowForm
    _ -> do
      setMessageI MsgFillAllRequired
      reshowForm

getBookR :: BookId -> Handler Html
getBookR bid = do
  Book { bookTitle = title, 
         bookAuthor = author,
         bookContentType = (decodeUtf8 -> contentType), 
         bookExternalLink = maybeUrl } <- runDB $ get404 bid
  let imageSrc = "http://s3.amazonaws.com/hbooks-static/img/" ++ showId bid :: Text
      fileUrl = 
        case maybeUrl of
          Just url -> url
          Nothing -> "/download/" ++ showId bid :: Text
  defaultLayout $(widgetFile "book")
      
getDownloadR :: BookId -> Handler Html
getDownloadR bid = redirect ("http://s3.amazonaws.com/hbooks-static/files/" ++ showId bid :: String)

bookForm :: Maybe Book -> [Tag] -> Form (
  Text, -- Title
  Text, -- Author
  BookType,
  BookCategory, 
  Maybe Text, -- Tags
  Maybe FileInfo, -- Thumbnail
  Maybe FileInfo, -- Upload file
  Maybe Text, -- URL
  Bool -- Download from URL
  )
bookForm maybeBook tags = renderDivs $ (,,,,,,,,)
  <$> areq textField (fieldSettingsLabel MsgTitle) (bookTitle <$> maybeBook)
  <*> areq textField (fieldSettingsLabel MsgAuthor) (bookAuthor <$> maybeBook)
  <*> areq (selectFieldList bookTypes) (fieldSettingsLabel MsgType) (bookType <$> maybeBook)
  <*> areq (selectFieldList bookCategories) (fieldSettingsLabel MsgCategory) (bookCategory <$> maybeBook)
  <*> aopt textField (fieldSettingsLabel MsgTags) (Just $ Just $ showTags tags)
  <*> fileAFormOpt (fieldSettingsLabel MsgThumbnail)
  <*> fileAFormOpt (fieldSettingsLabel MsgUpload)
  <*> aopt textField (fieldSettingsLabel MsgUrl) (bookExternalLink <$> maybeBook)
  <*> areq checkBoxField (fieldSettingsLabel MsgDownloadQ) (Just False)
    
getTags :: Maybe [TagId] -> Handler [Tag]
getTags (Just ids) = do
  listMaybeTags <- mapM (runDB . get) ids
  let tags = catMaybes listMaybeTags
  return tags
getTags Nothing = return []
    

