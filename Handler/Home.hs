{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = redirect BooksR

postLangR :: Handler ()
postLangR = do
  lang <- runInputPost $ ireq textField "lang"
  setLanguage lang
  redirect BooksR
