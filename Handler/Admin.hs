{-# LANGUAGE ViewPatterns #-}

module Handler.Admin where

import Import
import Yesod.Auth
import Data.Char (isAlphaNum)

requireAdmin = do
  Entity _ user <- requireAuth
  if userAdmin user then
    return ()
    else do
      setMessage "You're not an admin!"
      redirect BooksR
      

postAdminR :: Handler Html
postAdminR = do
  requireAdmin
  maybeMethod <- runInputPost $ iopt textField "method"
  case maybeMethod of
    Just "delete" -> do
      tag <- runInputPost $ ireq textField "tag"
      runDB $ deleteBy $ UniqueName tag
    _ ->  do 
      ((res, form), enctype) <- runFormPost addTagForm
      case res of 
        FormSuccess (filter isAlphaNum -> tag) -> do
          _ <- runDB $ insert $ Tag {tagName = tag}
          return ()
        _ -> setMessage "Form failed."
  redirect AdminR

getAdminR = do
  requireAdmin
  users <- runDB $ selectList [] [Asc UserId]
  tags <- runDB $ selectList [] [Asc TagId]
  ((res, form), enctype) <- runFormPost addTagForm
  defaultLayout $(widgetFile "admin")


addTagForm :: Form Text
addTagForm = renderDivs $ areq textField (fieldSettingsLabel MsgTag) Nothing 
