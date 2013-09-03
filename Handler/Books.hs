{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Handler.Books where

import Import

getBooksR :: Handler Html
getBooksR = do
  books <- runDB $ selectList [] [Asc BookId]
  defaultLayout $(widgetFile "books")

getAddBookR :: Handler Html
getAddBookR = do
  ((res, form), enctype) <- runFormPost bookForm
  case res of
    FormSuccess (title, cover) -> do
      let file = fileName cover
      liftIO $ toThumbnail cover
      cid <- runDB $ insert $ Cover file
      bid <- runDB $ insert $ Book title cid
      redirect $ BookR bid
    _ -> defaultLayout $(widgetFile "bookform")

postAddBookR :: Handler Html
postAddBookR = getAddBookR

getBookR :: BookId -> Handler Html
getBookR bid = do
  Book title cid <- runDB $ get404 bid
  maybeCover <- runDB $ get cid
  let imageSrc = 
        case maybeCover of
          Just (Cover imageFileName) -> "/static/img/covers/" ++ imageFileName
          Nothing -> ""
  defaultLayout $(widgetFile "book")
      
  

bookForm :: Form (Text,FileInfo)
bookForm = renderDivs $ (,)
    <$> areq textField "Title" Nothing
    <*> fileAFormReq "Cover image"
