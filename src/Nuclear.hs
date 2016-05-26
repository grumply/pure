{-# language GADTs #-}
module Nuclear where

type Header = String
type Body = String

data Msg where
  Msg :: Header -> Body -> Msg
  deriving (Show,Read,Eq,Ord)
