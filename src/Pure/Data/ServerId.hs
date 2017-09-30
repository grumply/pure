{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pure.Data.ServerId where

import Ef.Base

import Pure.Data.Micros
import Pure.Data.Millis
import Pure.Data.JSON
import Pure.Data.Txt

import Data.Unique

import Data.Hashable

import GHC.Generics

import System.IO.Unsafe
import System.Random

{-# NOINLINE startTimeMicros #-}
startTimeMicros :: Micros
startTimeMicros = unsafePerformIO micros

{-# NOINLINE startTimeMillis #-}
startTimeMillis :: Millis
startTimeMillis = unsafePerformIO millis

hashWithStartTime :: Hashable a => a -> Int
hashWithStartTime x = hash (getMillis startTimeMillis,x)

newtype ServerId = ServerId { getServerId :: Txt }
  deriving (Show,Generic,Eq,Ord,ToJSON,FromJSON)

instance ToTxt ServerId where
  toTxt = toTxt . getServerId

{-# NOINLINE originServer #-} -- noinlined unsafePerformIO
originServer :: Txt
originServer = toTxt $ unsafePerformIO (getRandString 12)

serverId :: ServerId
serverId = ServerId originServer

serverHash :: Int
serverHash = hash originServer

-- non-sequential server-mostly-unique integer generation
serverUniqueInt :: (MonadIO c) => c Int
serverUniqueInt = do
  u <- liftIO newUnique
  return $ hashWithSalt serverHash (hashUnique u)

getRandString :: Int -> IO String
getRandString n = do
  gen <- getStdGen
  let rs = randomRs ('a','z') gen
  return $ Prelude.take n rs

-- get_iface_inet :: String -> IO [String]
-- get_iface_inet iface = do
--   let cmd = "/sbin/ifconfig " ++ iface ++ " | grep 'inet addr' | awk -F: '{print $2}' | awk '{print $1}'"
--   (_, Just hout, _, _) <- createProcess (shell cmd) { std_out = CreatePipe }
--   Prelude.lines <$> hGetContents hout
