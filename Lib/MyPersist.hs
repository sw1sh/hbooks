module Lib.MyPersist where

import ClassyPrelude
import Prelude (read)
import Database.Persist.Sql

data BookType = Textbook | LectureNotes | OtherType
  deriving (Show, Read, Eq, Enum, Bounded)
data BookCategory = Physics | Mathematics | Humanities
  deriving (Show, Read, Eq, Enum, Bounded)

instance PersistFieldSql BookType where
  sqlType _ = SqlString

instance PersistFieldSql BookCategory where
  sqlType _ = SqlString

instance PersistField BookType where
  toPersistValue = PersistText . show
  fromPersistValue (PersistText text) = Right $ read (unpack text)

instance PersistField BookCategory where
  toPersistValue = PersistText . show
  fromPersistValue (PersistText text) = Right $ read (unpack text)

