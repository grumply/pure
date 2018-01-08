{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pure.Data.JSV where

import Pure.Data.Txt

#ifdef __GHCJS__
import GHCJS.Marshal.Pure
import GHCJS.Types

type JSV = JSVal
#else
type JSV = ()
#endif

newtype Win     = Win JSV
newtype Doc     = Doc JSV
newtype Loc     = Loc JSV
newtype Body    = Body JSV
newtype History = History JSV

newtype Element = Element JSV
newtype Text    = Text JSV
newtype Node    = Node JSV
newtype Frag    = Frag JSV

class IsNode e where
  toNode :: e -> Node
instance IsNode Node where
  toNode = id
instance IsNode Body where
  toNode (Body b) = Node b
instance IsNode Element where
  toNode (Element e) = Node e
instance IsNode Text where
  toNode (Text t) = Node t
instance IsNode Frag where
  toNode (Frag f) = Node f

class IsNode e => IsParent e
instance IsParent Element
instance IsParent Frag

-- Get rid of the IsJSV approach when JSString constructor is exposed.
-- Currently, I can't coerce JSString <-> JSVal without patching ghcjs-base.
class IsJSV a where
  toJSV :: a -> JSV
instance IsJSV JSV where
  toJSV = id
instance IsJSV Txt where
#ifdef __GHCJS__
  toJSV = pToJSVal
#else
  toJSV _ = ()
#endif
instance IsJSV Win where
  toJSV (Win w) = w
instance IsJSV Doc where
  toJSV (Doc d) = d
instance IsJSV Body where
  toJSV (Body b) = b
instance IsJSV Loc where
  toJSV (Loc l) = l
instance IsJSV Element where
  toJSV (Element e) = e
instance IsJSV Text where
  toJSV (Text t) = t
instance IsJSV Node where
  toJSV (Node n) = n
instance IsJSV History where
  toJSV (History h) = h
instance IsJSV Frag where
  toJSV (Frag f) = f

