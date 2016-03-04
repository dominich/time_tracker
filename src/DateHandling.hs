module DateHandling ( iso8601, fromISO8601 ) where

import Data.Time

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

fromISO8601 :: String -> Maybe UTCTime
fromISO8601 = parseTime defaultTimeLocale "%FT%T%QZ"



