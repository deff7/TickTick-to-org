module Convert where

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import Item


data Node = Leaf Item
          | Node (M.HashMap String Node)
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
insertItem (Node hm) it [] = Node $ M.insert (unTaskID it) (Leaf it) hm
insertItem (Node hm) it (x:xs) = Node $ M.alter update x hm
  where
    update (Just old) = Just $ insertItem old it xs
    update Nothing = Just $ (insertItem (Node M.empty) it xs)

itemsToTree :: V.Vector Item -> Node
itemsToTree items = V.foldl (\n it -> insertItem n it (itemPath it)) (Node M.empty) items
