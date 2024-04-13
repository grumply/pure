{-# language ScopedTypeVariables, LambdaCase, ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings, CPP, NamedFieldPuns, TypeApplications, RecordWildCards, StandaloneDeriving, DeriveGeneric, DeriveAnyClass, DerivingStrategies, DeriveFunctor, AllowAmbiguousTypes #-}
module Data.Log where

import Control.Applicative ((<|>))
import Control.Concurrent (newMVar,withMVar,readMVar,modifyMVar_,takeMVar)
import Control.Monad (void,mzero)
import Data.View (Exists,with,it,Producer,stream,(#),yield)
import Data.JSON (Value,ToJSON(..),FromJSON(..),pretty,encode,decode,logJSON)
import Data.Marker
import Data.Maybe (mapMaybe)
import Data.Time
import Data.Txt (Txt,FromTxt(..),ToTxt(..),intercalate,lines)
import GHC.Generics
import Prelude hiding (log,lines)

import GHC.Stack
import GHC.Exception (prettyCallStackLines)

import System.IO (SeekMode(..))
import System.Directory (createDirectoryIfMissing)
import qualified System.Posix.Files              as P
import qualified System.Posix.IO                 as P
#ifndef __GHCJS__
import qualified System.Posix.IO.ByteString.Lazy as P (fdWritev)
#endif
import           System.Posix.Types              as P (Fd, FileOffset, ByteCount, COff)

import System.FilePath

type Logging a = Producer (Event a)

data Critical = Warn | Error | Fatal
  deriving (Enum,Bounded,Eq,Ord,Generic)

instance ToJSON Critical where
  toJSON = toJSON @Txt . \case
    Warn  -> "WARN"
    Error -> "ERROR"
    Fatal -> "FATAL"

instance FromJSON Critical where
  parseJSON val = do
    t :: Txt <- parseJSON val
    case t of
      "WARN"  -> pure Warn
      "ERROR" -> pure Error
      "FATAL" -> pure Fatal
      _       -> mzero

data Informational = Trace | Debug | Info 
  deriving (Enum,Bounded,Eq,Ord,Generic)

instance ToJSON Informational where
  toJSON = toJSON @Txt . \case
    Trace -> "TRACE"
    Debug -> "DEBUG"
    Info  -> "INFO"

instance FromJSON Informational where
  parseJSON val = do
    t :: Txt <- parseJSON val
    case t of
      "TRACE" -> pure Trace
      "DEBUG" -> pure Debug
      "INFO"  -> pure Info
      _       -> mzero

data Level = Informational Informational | Critical Critical
  deriving (Eq,Ord,Generic)

noncritical :: Level -> Bool
noncritical (Critical _) = False
noncritical _ = True

noninformational :: Level -> Bool
noninformational (Informational _) = False
noninformational _ = True

instance ToJSON Level where
  toJSON = \case
    Informational i -> toJSON i
    Critical c -> toJSON c

instance FromJSON Level where
  parseJSON val = i <|> c
    where
      i = Informational <$> parseJSON val 
      c = Critical <$> parseJSON val

data Event a = Event Level a
  deriving stock (Generic,Eq,Ord,Functor)
  deriving anyclass (ToJSON,FromJSON)

logger :: (Level -> a -> IO ()) -> (Logging a => x) -> x
logger onLog = stream (\(Event lvl e) -> onLog lvl e)

sublogger :: (a -> b) -> (Logging a => x) -> (Logging b => x)
sublogger f = ((fmap @Event f) #)

logging :: forall a x. Level -> (Producer a => x) -> (Logging a => x)
logging level = (f #) -- loggingWith level (id @a)
  where
    f :: a -> Event a
    f = Event level

-- A convenience for the combination of `sublogger` and `logging`
loggingWith :: Level -> (a -> b) -> (Producer a => x) -> (Logging b => x)
loggingWith level f = ((Event level . f) #)

-- Modify a logger to ignore levels via predicate.
--
-- > logger (printer & ignoring noncritical)
--
ignoring 
  :: (Level -> Bool) 
  -> (Level -> a -> IO ()) 
  -> (Level -> a -> IO ())
ignoring predicate onLog lvl val 
  | predicate lvl = pure ()
  | otherwise     = onLog lvl val

-- Amend an in-flight logging event by effectuflly analyzing and modifying the
-- level and value.
--
-- > logger (printer & amending (\level value -> ...) & ignoring noncritical)
--
amending
  :: (Level -> a -> IO (Level,a)) 
  -> (Level -> a -> IO ()) 
  -> (Level -> a -> IO ())
amending f onLog lvl val = do
  evt <- f lvl val
  uncurry onLog evt

-- Minimal source location of `<function>@<module>:<line>::...` in text format.
location :: HasCallStack => Txt
location = fromCallStack callStack
  where
    fromCallStack :: CallStack -> Txt
    fromCallStack cs = intercalate "::" (fmap fromCallSite (getCallStack cs))
    
    fromCallSite (f,SrcLoc { srcLocModule, srcLocStartLine }) = 
      toTxt f <> "@" <> toTxt srcLocModule <> ":" <> toTxt srcLocStartLine 

log :: Logging a => Level -> a -> IO ()
log lvl a = yield (Event lvl a)

informational :: Logging a => Informational -> a -> IO ()
informational = log . Informational

critical :: Logging a => Critical -> a -> IO ()
critical = log . Critical

trace :: Logging a => a -> IO ()
trace = informational Trace

debug :: Logging a => a -> IO ()
debug = informational Debug

info :: Logging a => a -> IO ()
info = informational Info

warn :: Logging a => a -> IO ()
warn = critical Warn

error :: Logging a => a -> IO ()
error = critical Error

fatal :: Logging a => a -> IO ()
fatal = critical Fatal

-- Simple printing logger.
printer :: ToJSON a => Level -> a -> IO ()
printer level value = do
  now <- time 
  putStrLn (fromTxt (encode (level,RFC3339 now,value)))

-- Thread-safe version of `printer`.
printer' :: ToJSON a => IO (Level -> a -> IO ())
printer' = do
  mv <- newMVar ()
  pure (\lvl val -> withMVar mv (\_ -> printer lvl val))

-- Pretty-printing logger.
pPrinter :: ToJSON a => Level -> a -> IO ()
pPrinter level value = do
  now <- time
  logJSON (toJSON (level,RFC3339 now,value))

-- | Thread-safe version of `pPrinter`.
pPrinter' :: ToJSON a => IO (Level -> a -> IO ())
pPrinter' = do
  mv <- newMVar ()
  pure (\lvl val -> withMVar mv (\_ -> pPrinter lvl val))

data Record = Record
  { dir     :: FilePath
  , file    :: FilePath
  , opened  :: Time
  , latest  :: Time
  , written :: Int
  }

newtype Policy = Policy (Record -> IO (Maybe FilePath))

instance Monoid Policy where
  mempty = Policy (\_ -> pure Nothing)
  mappend = (<>)
  
instance Semigroup Policy where
  (<>) (Policy p1) (Policy p2) = Policy p3
    where
      p3 r = do
        mfp1 <- p1 r
        case mfp1 of
          Nothing -> p2 r
          Just fp -> pure (Just fp)

newLogFile :: IO FilePath
newLogFile = do
  m <- markIO
  pure (fromTxt (toTxt m) <> ".log")

-- Rotate log every `duration`.
--
-- > recording "root/directory" (mempty & rotatingEvery Hour) ...
rotatingEvery :: Time -> Policy -> Policy
rotatingEvery duration (Policy p) = Policy p'
  where
    p' record@Record {..}
      | latest - opened < duration = p record
      | otherwise = Just <$> newLogFile

-- Rotate log monthly.
--
-- > recording "root/directory" (mempty & rotatingMonthly) ...
rotatingMonthly :: Policy -> Policy
rotatingMonthly (Policy p) = Policy p'
  where
    p' record@Record {..}
      | Months m _ <- opened, Months n _ <- latest, m < n = Just <$> newLogFile
      | otherwise = p record

-- Rotate log daily.
--
-- > recording "root/directory" (mempty & rotatingDaily) ...
rotatingDaily :: Policy -> Policy
rotatingDaily (Policy p) = Policy p'        
  where
    p' record@Record {..}
      | Days m _ <- opened, Days n _ <- latest, m < n = Just <$> newLogFile
      | otherwise = p record

-- Rotate log after `bytes` written.
--
-- > recording "root/directory" (mempty & rotatingAfter 1e7) ...
rotatingAfter :: Int -> Policy -> Policy
rotatingAfter bytes (Policy p) = Policy p'
  where
    p' record@Record {..}
      | written > bytes = Just <$> newLogFile
      | otherwise = p record

-- | Thread-safe file logging. To read the log lines, use `recorded`.
recording :: ToJSON a => FilePath -> Policy -> IO (Level -> a -> IO (),IO ()) 
recording dir (Policy p) = do
  createDirectoryIfMissing True dir
  m <- markIO
  let file = fromTxt (toTxt m) <> ".log"
  now <- time
  fd <- P.openFd (dir </> file) P.ReadWrite (Just $ P.unionFileModes P.ownerReadMode P.ownerWriteMode) P.defaultFileFlags
  mv <- newMVar (Record dir file now now 0,fd)
  P.fdSeek fd SeekFromEnd 0
  pure (logger mv,takeMVar mv >> P.closeFd fd) 
  where
    logger mv level value =
      modifyMVar_ mv $ \(rcrd,fd) -> do
        mfp <- p rcrd
        case mfp of
          Nothing -> do
            now <- time
            bytes <- P.fdWrite fd (fromTxt (encode (level,RFC3339 now,value)) <> "\n")
            pure (rcrd { latest = now, written = written rcrd + fromIntegral bytes },fd)
          Just fp -> do
            P.closeFd fd
            fd <- P.openFd fp P.ReadWrite (Just $ P.unionFileModes P.ownerReadMode P.ownerWriteMode) P.defaultFileFlags   
            now <- time
            bytes <- P.fdWrite fd (fromTxt (encode (level,RFC3339 now,value)) <> "\n")
            pure (rcrd { opened = now, latest = now, written = fromIntegral bytes, file = fp },fd)

recorded :: forall a. FromJSON a => FilePath -> IO [(Level,Time,a)]
recorded = fmap (mapMaybe decode . lines . toTxt) . readFile

memory :: IO (Level -> a -> IO (),IO [(Level,Time,a)])
memory = do
  mv <- newMVar []
  pure (logger mv,reverse <$> readMVar mv)
  where
    logger mv level value =
      modifyMVar_ mv $ \rcrds -> do
        now <- time
        pure ((level,now,value):rcrds)
