module Nuclear.Try where

import Data.JSText
import GHC.Generics

data Try a
  = Trying
  | Failed
  | Done a
  deriving (Eq,Generic,ToJSON,FromJSON)

isTrying :: Try a -> Bool
isTrying Trying = True
isTrying _ = False

isFailed :: Try a -> Bool
isFailed Failed = True
isFailed _ = False

isDone :: Try a -> Bool
isDone (Done _) = True
isDone _ = False
