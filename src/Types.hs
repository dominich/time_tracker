module Types (
  Issue(..)
, StartedTask(..)
, CompletedTask(..)
) where

import Data.Time

type Issue = Maybe Int
data StartedTask = StartedTask UTCTime Issue String
  deriving (Eq, Show)
data CompletedTask = CompletedTask StartedTask UTCTime
  deriving (Eq, Show)
