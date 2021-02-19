{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Item where

import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Csv
import Data.List.Split
import Data.Char
import Control.Monad
import qualified Data.ByteString.Char8 as BS

data Status = Normal
            | Completed
            | Archived
            deriving (Show, Eq, Ord)

data Item = Item { unFolderName :: Maybe String
                 , unListName :: String
                 , unTitle :: String
                 , unTags :: [String]
                 , unIsCheckList :: Bool
                 , unContent :: String
                 , unDueDate :: Maybe UTCTime
                 , unCreatedTime :: UTCTime
                 , unCompletedTime :: Maybe UTCTime
                 , unRepeat :: String -- TODO: handle this field
                 , unColumnName :: Maybe String
                 , unTaskID :: String
                 , unChildTasks :: [Item]
                 , unStatus :: Status
                 } deriving (Show, Eq)

instance FromField Status where
  parseField s
    | s == "0" = pure Normal
    | s == "1" = pure Completed
    | s == "2" = pure Archived
    | otherwise = mzero

instance FromField UTCTime where
  -- 2021-02-10T21:00:00+0000
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" . BS.unpack

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item <$>
      m .: "Folder Name" <*>
      m .: "List Name" <*>
      m .: "Title" <*>
      m .: "Tags" <*>
      m .: "Is Check list" <*>
      m .: "Content" <*>
      m .: "Due Date" <*>
      m .: "Created Time" <*>
      m .: "Completed Time" <*>
      m .: "Repeat" <*>
      m .: "Column Name" <*>
      m .: "taskId" <*>
      pure [] <*>
      m .: "Status"

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
