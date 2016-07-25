{-# language NoMonomorphismRestriction #-}
{-# language FlexibleContexts #-}
module Nuclear (module Nuclear,module Data.Time) where

import Ef.Base

import Data.Time

import qualified Data.IntMap.Strict as IM

type Header = String
type Body = String

data Msg
  = Msg Header Body
  deriving (Show,Read,Eq,Ord)

data Traced
  = Traced (Integer,Double) Int String
  deriving (Read,Show,Eq,Ord)

createTraceKey = do
  now <- liftIO getCurrentTime
  let l = toModifiedJulianDay (utctDay now)
      r = fromRational $ toRational (utctDayTime now)
  return (l,r)

data Tracing
  = Tracing (Integer,Double) Int
  | NotTracing

tracing = state NotTracing

getTraceKey = do
  t <- get
  return $
    case t of
      Tracing k d -> Just (k,d)
      NotTracing -> Nothing

enableTracing k d = put (Tracing k d)
disableTracing = put NotTracing

traced = do
  k <- createTraceKey
  return (Traced k 0)

data TraceSpan
  = TraceSpan
    { traceSpanId :: (Integer,Double)
    , traceSpanStart :: (Integer,Double)
    , traceSpanEnd :: (Integer,Double)
    , traceSpanCurrentDepth :: Int
    , traceSpanSource :: Maybe String
    , traceSpanAnnotation :: Maybe String
    }
  deriving (Read,Show)

traces = "/traces"
data Traces = Traces String Int [TraceSpan]
  deriving (Read,Show)

data TraceNode
  = TraceRootNode
    { traceId :: (Integer,Double)
    , traceStart :: (Integer,Double)
    , traceEnd :: (Integer,Double)
    , traceLevels :: IM.IntMap [TraceNode]
    , traceHost :: String
    , traceSource :: Maybe String
    , traceAnnotation :: Maybe String
    }
  | TraceNode
    { traceNodeStart :: (Integer,Double)
    , traceNodeEnd :: (Integer,Double)
    , traceNodeHost :: String
    , traceNodeSource :: Maybe String
    , traceNodeAnnotation :: Maybe String
    }
  deriving (Read,Show)

