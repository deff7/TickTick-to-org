{-# LANGUAGE OverloadedStrings #-}

module Convert where

import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Sort (sortBy)
import Data.Function (on)
import Data.List.Split (splitOn)

import Item


data Node = Leaf Int Item
          | Node Int String (M.HashMap String Node)
          deriving (Show, Eq)

itemPath :: Item -> [String]
itemPath it = concat . map (\f -> f it) $
  [ \it -> case unFolderName it of
             Nothing -> []
             Just n -> [n]
  , \it -> [unListName it]
  , \it -> case unColumnName it of
             Nothing -> []
             Just n -> if n == "Not Sectioned" then [] else [n]
  ]

insertItem :: Node -> Item -> [String] -> Node
insertItem (Node lvl name hm) it [] = Node lvl name $ M.insert (unTaskID it) (Leaf (lvl+1) it) hm
insertItem (Node lvl name hm) it (x:xs) = Node lvl name $ M.alter update x hm
  where
    update (Just old) = Just $ insertItem old it xs
    update Nothing = Just $ (insertItem (Node (lvl+1) x M.empty) it xs)

itemsToTree :: V.Vector Item -> Node
itemsToTree = V.foldl (\n it -> insertItem n it (itemPath it)) (Node 0 "*" M.empty)

convertContent :: String -> Blocks
convertContent = mconcat . map (para . text . T.pack) . splitOn "\r"

treeToPandoc :: Node -> Pandoc
treeToPandoc root = setTitle "Root" $ doc $ go root
  where
    go :: Node -> Blocks
    go (Leaf lvl it) = header lvl (spanWith attrs txt) <> scheduled <> contents
      where
        attrs = ("", if unStatus it == Normal then ["todo", "TODO"] else ["done", "DONE"], [])
        prefix = if unStatus it == Normal then "TODO " else "DONE "
        txt = text $ prefix <> (T.pack $ trimSpace $ unTitle it)
        scheduled = maybe mempty (\dt -> para $ str "SCHEDULED") (unDueDate it)
        contents = convertContent $ unContent it
    go (Node lvl title hm) = hdr <> next
      where
        hdr =
          if lvl > 0
          then header lvl (text $ T.pack title)
          else mempty
        next = foldl1 (<>) (map go elems)
        elems = sortBy f $ M.elems hm

        f (Leaf _ a) (Leaf _ b) = (compare `on` unStatus) a b
        f _ _ = EQ

debug :: Pandoc
debug = doc $
  header 1 (spanWith ("", [], []) (text "Test")) <>
  header 1 (spanWith ("", [], []) (text "Test")) <>
  header 1 (spanWith ("", [], []) (text "Test"))
