{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns, RecordWildCards #-}

module Handler.Books where

import Import
import Yesod.Auth
import Data.Conduit (runResourceT, ($$))
import Data.Conduit.Binary (sinkLbs)
import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

requireOwner bid = do
  owner@(Entity uid user) <- requireAuth
  book <- runDB $ get404 bid
  if uid == bookOwner book || userAdmin user then
    return owner
    else do
      setMessage "Access restricted!"
      redirect $ BookR bid

getBooksR :: Handler Html
getBooksR = do
  tags <- runDB $ selectList [] [Asc TagId]
  defaultLayout $(widgetFile "books")

getAddBookR :: Handler Html
getAddBookR = do
  let book = Nothing
      tags = []
  defaultLayout $(widgetFile "bookform")

postAddBookR :: Handler Html
postAddBookR = modifyBookR Nothing

getEditBookR :: BookId -> Handler Html
getEditBookR bid = do
  requireOwner bid
  book <- runDB $ get bid
  tags <- getTags $ bookTags <$> book
  defaultLayout $(widgetFile "bookform")

postEditBookR :: BookId -> Handler Html
postEditBookR bid = modifyBookR $ Just bid

postBookR :: BookId -> Handler Html
postBookR bid = do
  book <- runDB $ get404 bid
  Entity uid user <- requireOwner bid
  method <- runInputPost $ ireq textField "method"
  case method of
    "delete" -> do
      runDB $ delete bid 
      setMessage "Book successfully deleted!"
      redirect BooksR
    _ -> error "Unknown method"

modifyBook uid maybeBid book =
  case maybeBid of
    Just bid -> do
      requireOwner bid
      runDB $ replace bid book
      return bid
    Nothing -> do
      runDB $ insert book

modifyBookR maybeBid = do
  Entity uid _ <- 
    case maybeBid of
      Just bid -> requireOwner bid
      Nothing -> requireAuth
  maybeBook <- maybe (return Nothing) (runDB . get) maybeBid
  let backR = 
        case maybeBid of
          Just bid -> EditBookR bid
          Nothing -> AddBookR
  ( title, 
    author, 
    typ, 
    category,
    toTags -> tags,
    maybeThumb, 
    maybeThumbUrl,
    maybeFile, 
    maybeUrl, 
    isDownload ) <- runInputPost bookForm 
      
  maybeTags <- mapM (runDB . getByValue) tags
  let tagIds = map entityKey $ catMaybes maybeTags
      finishAction bid = do
        case maybeBook of
          Just b | bookTitle b == title 
                    && isNothing maybeThumb && isNothing maybeThumbUrl 
                      -> redirect $ BookR bid
          _ ->
            case maybeThumbUrl of
              Just url -> do
                res <- liftIO $ downloadFile url
                case res of
                  Right (fromString -> contentType, _, blob) -> do
                    liftIO $ saveThumbnail (Just (contentType, blob)) title $ showId bid
                    redirect $ BookR bid
                  Left err -> do
                    setMessage $ toHtml err
                    redirect backR
              Nothing -> do
                thumb <- 
                  case maybeThumb of
                    Just fi -> do
                      blob <- runResourceT $ fileSource fi $$ sinkLbs
                      return $ Just (fileContentType fi, repack blob)
                    Nothing -> return Nothing
                liftIO $ saveThumbnail thumb title $ showId bid
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
              redirect backR
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
                                   BookAuthor =. author,
                                   BookType =. typ, 
                                   BookCategory =. category, 
                                   BookTags =. tagIds ]
              finishAction bid
            Nothing -> do
              setMessageI MsgUploadOrProvideUrl
              redirect backR  

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

bookForm = (,,,,,,,,,)
  <$> ireq textField "title"
  <*> ireq textField "author"
  <*> ireq (selectFieldList bookTypes) "booktype"
  <*> ireq (selectFieldList bookCategories) "bookcategory"
  <*> iopt textField "tags"
  <*> iopt fileField "thumbnail"
  <*> iopt urlField "thumburl"
  <*> iopt fileField "uploadfile"
  <*> iopt urlField "fileurl"
  <*> ireq checkBoxField "downloadQ"

getTags :: Maybe [TagId] -> Handler [Tag]
getTags (Just ids) = do
  listMaybeTags <- mapM (runDB . get) ids
  let tags = catMaybes listMaybeTags
  return tags
getTags Nothing = return []
    
checkMaybe f t = maybe False ((== t) . f)
