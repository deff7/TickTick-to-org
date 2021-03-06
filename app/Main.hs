{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Csv
import qualified Data.Vector as V

import Text.Pandoc
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Map (fromList)
import Item
import Convert
import Display


-- Pandoc (Meta {unMeta = fromList [("title",MetaInlines [Str "Example"])]}) [Header 1 ("",[],[]) [Str "Inbox"],Header 2 ("",[],[]) [Str "Sub-folder"],Header 3 ("",[],[]) [Span ("",["todo","TODO"],[]) [Str "TODO"],Space,Str "Item",Space,Str "1"],Para [Str "Content"],Header 3 ("",[],[]) [Span ("",["todo","TODO"],[]) [Str "TODO"],Space,Str "Item",Space,Str "2"],Header 3 ("",[],[]) [Span ("",["done","DONE"],[]) [Str "DONE"],Space,Str "Item",Space,Str "3"]]

saveToOrg :: FilePath -> V.Vector Item -> IO ()
saveToOrg f items = do
  txt <- runIOorExplode (writeOrg (def { writerWrapText = WrapNone }) $ treeToPandoc $ itemsToTree items)
  T.writeFile f txt

myDoc :: Pandoc
myDoc = Pandoc (Meta {unMeta = fromList [("title",
          MetaInlines [Str "My",Space,Str "title"])]})
        [Para [Str "This",Space,Str "is",Space,Str "the",Space,Str "first",
         Space,Str "paragraph"],Para [Str "And",Space,Emph [Str "another"],
         Str "."]
        ,BulletList [
          [Para [Str "item",Space,Str "one"]
          ,Para [Str "continuation"]]
         ,[Plain [Str "item",Space,Str "two",Space,Str "and",Space,
                  Str "a",Space,Link nullAttr [Str "link"] ("/url","go to url")]]]]

main :: IO ()
main = do
  f <- BS.readFile "/home/deff/Downloads/TickTick-backup-2021-02-14.csv"
  runIOorExplode (writeOrg def myDoc) >>= T.writeFile "out2.org"
  let input = BS.unlines $ drop 6 $ BS.lines f
  case decodeByName input :: Either String (Header, V.Vector Item) of
    Left e -> print e
    Right (_, items) -> saveToOrg "test.org" items
