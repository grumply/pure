module Nuclear where

type Header = String
type Body = String

data Msg
  = Msg Header Body
  deriving (Show,Read,Eq,Ord)
