module Pure.Convoker.Discussion.Shared.Ago (ago) where

import Pure

import System.IO.Unsafe

-- | Given a time, create a pretty `<time> ago` string. This method is
-- slightly incorrect at boundaries, e.g.
--
-- > ago (now - Weeks 3 (Days 2 0)) == "3 weeks 1 day ago"
-- > ago (now - Weeks 3 (Days 2 (Seconds 1 0))) == "3 weeks 2 days ago"
--
ago :: Time -> Time -> Txt
ago now t
  | Seconds s _ <- now - t, s < 60 = "just now"
  | Minutes m _ <- now - t, m < 5  = "recently"
  | otherwise                      = go
  where
    go
      | ds >= 365  = years 
      | ds >= 30   = months
      | ds >= 7    = weeks
      | hs >= 24   = days  
      | ms >= 60   = hours
      | otherwise = minutes
      where
        ds = ms / 1440
        hs = ms / 60
        Minutes ms _ = now - t

    ns x nm 
      | x == 0 = ""
      | x == 1 = "1 " <> nm
      | otherwise = toTxt (round x :: Int) <> " " <> nm <> "s"

    years = 
      let Years ys (Months ms _) = now - t
      in ns ys "year" <> " " <> ns ms "month" <> " ago"
    
    months =
      let Months ms (Weeks ws _) = now - t
      in ns ms "month" <> " " <> ns ws "week" <> " ago"

    weeks =
      let Weeks ws (Days ds _) = now - t
      in ns ws "week" <> " " <> ns ds "day" <> " ago"

    days =
      let Days ds (Hours hs _) = now - t
      in ns ds "day" <> " " <> ns hs "hour" <> " ago"

    hours =
      let Hours hs (Minutes ms _) = now - t
      in ns hs "hour" <> " " <> ns ms "minute" <> " ago"

    minutes =
      let Minutes ms _ = now - t
      in toTxt (round ms :: Int) <> " minutes ago"