module Convert where

import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as S

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
itemsToTree items = V.foldl (\n it -> insertItem n it (itemPath it)) (Node 0 "*" M.empty) items

treeToPandoc :: Node -> Pandoc
treeToPandoc root = doc $ go root
  where
    go :: Node -> Blocks
    go (Leaf lvl it) = header lvl (text $ T.pack $ unTitle it)
    go (Node lvl title hm) = header lvl (text $ T.pack title) <> next
      where
        next = foldl1 (<>) (map go (M.elems hm))
