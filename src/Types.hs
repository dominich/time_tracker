module Types (
  Issue(..)
, StartedTask(..)
, CompletedTask(..)
) where

import Data.Time

type Issue = Maybe Int
data StartedTask = StartedTask { startTime :: UTCTime
                               , issue :: Issue
                               , description :: String } deriving (Eq, Show)
data CompletedTask = CompletedTask { startedTask :: StartedTask
                                   ,  endTime :: UTCTime } deriving (Eq, Show)
