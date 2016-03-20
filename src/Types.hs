module Types (
  Issue(..)
, StartedTask(..)
, CompletedTask(..)
) where

import Data.Time

data Issue = Issue Int | NoIssue
  deriving (Eq, Show)
data StartedTask = StartedTask UTCTime Issue String
  deriving (Eq, Show)
data CompletedTask = CompletedTask StartedTask UTCTime
  deriving (Eq, Show)
