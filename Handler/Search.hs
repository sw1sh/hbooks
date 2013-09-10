{-# LANGUAGE ViewPatterns #-}

module Handler.Search where

import Import
import Prelude (read)
import Database.Persist.Sql

like field val = Filter field (Left $ concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")


getSearchR :: Handler Value
getSearchR = do
  maybeQ <- lookupGetParam "q"
  maybeC <- lookupGetParam "c"
  maybeT <- lookupGetParam "t"
  let queryF = 
        case maybeQ of
          Just q -> FilterOr [like BookTitle q, like BookAuthor q]
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

  let takeId (Entity id _) = id
  (map takeId -> tagIds) <- runDB $ selectList [TagName <-. tagNames] []
  
  let bookFilter = [ queryF, BookCategory <-. categories ]
                     
  
  books <- runDB $ selectList bookFilter [Asc BookId]
  returnJson $ 
    if null tagIds then 
      books 
      else filter (any (`elem`tagIds) . bookTags . entityVal) books

translate dict get set x = 
  case lookup (get x) dict of
    Just v -> set x v 
    Nothing -> x
