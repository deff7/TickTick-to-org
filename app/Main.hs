{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Csv
import qualified Data.Vector as V

import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text.IO as T

import Item
import Convert
import Display


debug :: Text -> IO ()
debug txt = runIOorExplode (readOrg def txt) >>= print

-- Pandoc (Meta {unMeta = fromList [("title",MetaInlines [Str "Example"])]}) [Header 1 ("",[],[]) [Str "Inbox"],Header 2 ("",[],[]) [Str "Sub-folder"],Header 3 ("",[],[]) [Span ("",["todo","TODO"],[]) [Str "TODO"],Space,Str "Item",Space,Str "1"],Para [Str "Content"],Header 3 ("",[],[]) [Span ("",["todo","TODO"],[]) [Str "TODO"],Space,Str "Item",Space,Str "2"],Header 3 ("",[],[]) [Span ("",["done","DONE"],[]) [Str "DONE"],Space,Str "Item",Space,Str "3"]]

saveToOrg :: FilePath -> V.Vector Item -> IO ()
saveToOrg f items = runIOorExplode (writeOrg def $ treeToPandoc $ itemsToTree items) >>= T.writeFile f

main :: IO ()
main = do
  -- T.readFile "./test/data/example.org" >>= debug
  f <- BS.readFile "/home/deff/Downloads/TickTick-backup-2021-02-14.csv"
  let input = BS.unlines $ drop 6 $ BS.lines f
  case decodeByName input :: Either String (Header, V.Vector Item) of
    Left e -> print e
    Right (_, items) -> saveToOrg "test.org" items
