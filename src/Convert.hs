{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Convert where

import Data.Time
import Data.Time.Format
import Data.Csv
import Data.List.Split
import Data.Char
import Control.Monad
import qualified Data.ByteString.Char8 as BS

data Status = Normal
            | Completed
            | Archived
            deriving Show

data Item = Item { unTitle :: String
                 , unTags :: [String]
                 , unIsCheckList :: Bool
                 , unContent :: String
                 , unDueDate :: Maybe UTCTime
                 , unRepeat :: String -- TODO: handle this field
                 , unColumnName :: Maybe String
                 , unTaskID :: Int
                 , unChildTasks :: [Item]
                 } deriving Show

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item <$>
      m .: "Title" <*>
      m .: "Tags" <*>
      m .: "Is Check list" <*>
      m .: "Content" <*>
      m .: "Due Date" <*>
      m .: "Repeat" <*>
      m .: "Column Name" <*>
      m .: "taskId" <*>
      pure []

trimSpace :: String -> String
trimSpace = f . f
  where f = reverse . dropWhile isSpace

instance FromField [String] where
  parseField = pure . map trimSpace . splitOn "," . BS.unpack

instance FromField Bool where
  parseField s
    | s == "Y" = pure True
    | s == "N" = pure False
    | otherwise = mzero

instance FromField UTCTime where
  -- 2021-02-10T21:00:00+0000
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" . BS.unpack

data List = List String [Item] deriving Show

data Folder = Folder String [List] deriving Show
