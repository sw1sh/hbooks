module Import
    ( module Import
    ) where

import           ClassyPrelude        as Import hiding (delete, insert)
import           Yesod                as Import hiding (Route (..))
import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

bookTypes = [
  (MsgTextbook, Textbook),
  (MsgLectureNotes, LectureNotes),
  (MsgOtherType, OtherType)
  ]

bookCategories = [
  (MsgPhysics, Physics),
  (MsgMathematics, Mathematics),
  (MsgHumanities, Humanities)
  ]
