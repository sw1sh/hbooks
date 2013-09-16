{-# LANGUAGE ViewPatterns #-}

module Handler.Admin where

import Import
import Data.Char (isAlphaNum)

postAdminR :: Handler Html
postAdminR = do
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
  users <- runDB $ selectList [] [Asc UserId]
  tags <- runDB $ selectList [] [Asc TagId]
  ((res, form), enctype) <- runFormPost addTagForm
  defaultLayout $(widgetFile "admin")


addTagForm :: Form Text
addTagForm = renderDivs $ areq textField (fieldSettingsLabel MsgTag) Nothing 
