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
      eitherTags <- mapM (runDB . insertBy) tags
      let takeId (Left (Entity id _)) = id
          takeId (Right id) = id
          tagIds = map takeId eitherTags
          finishAction bid = do
            liftIO $ saveThumbnail maybeThumb title $ showId bid
            redirect $ BookR bid
      case maybeFile of
        Just file -> do    
          bid <- modifyBook maybeBid 
            $ Book { bookTitle = title,
                     bookAuthor = author,
                     bookContentType = fromString . unpack $ fileContentType file,
                     bookContentExtension = getFileExt $ fileName file,
                     bookExternalLink = Nothing,
                     bookType = typ,
                     bookCategory = category,
                     bookTags = tagIds }
          content <- runResourceT $ fileSource file $$ sinkLbs
          liftIO $ saveFile (repack content) (showId bid)
          finishAction bid
        Nothing -> 
          case (maybeUrl, isDownload) of
            (Just url, True) -> do
              res <- liftIO $ downloadFile url
              case res of
                Right (contentType, ext, body) -> do
                  bid <- modifyBook maybeBid 
                    $ Book { bookTitle = title,
                             bookAuthor = author,
                             bookContentType = fromString contentType,
                             bookContentExtension = fromString ext, 
                             bookExternalLink = Nothing,
                             bookType = typ,
                             bookCategory = category,
                             bookTags = tagIds }
                  liftIO $ saveFile body $ showId bid
                  finishAction bid
                Left err -> do
                  setMessage $ toHtml err
                  reshowForm
            (Just url, False) -> do
              bid <- modifyBook maybeBid 
                $ Book { bookTitle = title,
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
                  setMessage "Upload file or provide a link."
                  reshowForm
    _ -> do
      setMessage "Fill all required fields."
      reshowForm

getBookR :: BookId -> Handler Html
getBookR bid = do
  Book { bookTitle = title, 
         bookAuthor = author,
         bookContentType = decodeUtf8 -> contentType, 
         bookExternalLink = maybeUrl } <- runDB $ get404 bid
  let imageSrc = "/static/img/thumbs/" ++ showId bid :: Text
      fileUrl = 
        case maybeUrl of
          Just url -> url
          Nothing -> "/download/" ++ showId bid :: Text
  defaultLayout $(widgetFile "book")
      
getDownloadR :: BookId -> Handler Html
getDownloadR bid = do
  Book { bookTitle = title, 
         bookContentType = contentType, 
         bookContentExtension = ext, .. } <- runDB $ get404 bid
  let file = ResponseFile ok200 
        [ ("Content-Disposition", "filename=" ++ encodeUtf8 ("\"" ++ title ++ "." ++ ext ++ "\"") ), 
          (hContentType, contentType)] 
        ("static/books/" ++ showId bid) Nothing
  sendWaiResponse file

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
  <$> areq textField "Title" (bookTitle <$> maybeBook)
  <*> areq textField "Author" (bookAuthor <$> maybeBook)
  <*> areq (selectFieldList bookTypes) "Type" (bookType <$> maybeBook)
  <*> areq (selectFieldList bookCategories) "Category" (bookCategory <$> maybeBook)
  <*> aopt textField "Tags" (Just $ Just $ showTags tags)
  <*> fileAFormOpt "Thumbnail image"
  <*> fileAFormOpt "Upload file"
  <*> aopt textField "File URL" (bookExternalLink <$> maybeBook)
  <*> areq checkBoxField "Download?" (Just False)
    
getTags :: Maybe [TagId] -> Handler [Tag]
getTags (Just ids) = do
  listMaybeTags <- mapM (runDB . get) ids
  let tags = catMaybes listMaybeTags
  return tags
getTags Nothing = return []
    

