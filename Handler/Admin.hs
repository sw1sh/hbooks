module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
  users <- runDB $ selectList [] [Asc UserId]
  tags <- runDB $ selectList [] [Asc TagId]
  defaultLayout $(widgetFile "admin")
