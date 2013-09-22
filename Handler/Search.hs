{-# LANGUAGE ViewPatterns #-}

module Handler.Search where

import Import
import Text.Hamlet
import Prelude (read)
import Database.Persist.Sql

like field val = Filter field (Left val) (BackendSpecificFilter "ILIKE")


getSearchR :: Handler TypedContent
getSearchR = do
  maybeE <- lookupGetParam "e"
  case maybeE of
    Just "tags" ->
      tagSearch
    _ ->
      bookSearch

tagSearch = do
  maybeQ <- lookupGetParam "q"
  let tagFilter =
        case maybeQ of
          Just q -> [like TagName $ q ++ "%"]
          Nothing -> []
  tags <- runDB $ selectList tagFilter [Asc TagName]
  selectRep $
    provideRep $ returnJson tags

bookSearch = do
  maybeQ <- lookupGetParam "q" -- search query
  maybeC <- lookupGetParam "c" -- category
  maybeT <- lookupGetParam "t" -- tags
  let queryF = 
        case maybeQ of
          Just q -> FilterOr [like BookTitle $ "%"++q++"%", like BookAuthor $ "%"++q++"%"]
          Nothing -> FilterOr []
      categories =
        case maybeC of
          Just "" -> map snd bookCategories
          Just c -> map (read . unpack) $ words c :: [BookCategory]
          Nothing -> map snd bookCategories
      tagNames = 
        case maybeT of
          Just t -> words t
          Nothing -> []

  (map entityKey -> tagIds) <- runDB $ selectList [TagName <-. tagNames] []
  
  let bookFilter = [ queryF, BookCategory <-. categories ]
  
  books' <- runDB $ selectList bookFilter [Asc BookId]
  let books = if null tagIds then books'
              else filter (any (`elem`tagIds) . bookTags . entityVal) books'
  (map (\(Entity tid tag) -> (tid, tagName tag)) -> tags) <- runDB $ selectList [] [Asc TagId]
  selectRep $ do
    provideRep $ returnJson $ books
    provideRep $ giveUrlRenderer $(hamletFile "templates/search.hamlet")
