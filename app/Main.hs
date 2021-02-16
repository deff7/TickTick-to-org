module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Csv
import qualified Data.Vector as V

import Convert

main :: IO ()
main = do
  f <- BS.readFile "/home/deff/Downloads/TickTick-backup-2021-02-14.csv"
  let input = BS.unlines $ drop 6 $ BS.lines f
  BS.putStrLn input
  case decodeByName input :: Either String (Header, V.Vector Item) of
    Left e -> print e
    Right (_, items) -> print $ V.map ((,) <$> unDueDate <*> unTaskID) items
