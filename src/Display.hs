module Display (displayTree) where

import Data.Tree (unfoldTree, Tree, drawTree)
import qualified Data.HashMap.Strict as M

import Item
import Convert

displayTree :: Node -> IO ()
displayTree = putStrLn . drawTree . buildTree

buildTree :: Node -> Tree String
buildTree root = unfoldTree f root
  where
    f (Leaf _ it) = (brief $ (unTaskID it) <> " " <> (unTitle it), [])
    f (Node _ name hm) = (name, M.elems hm)

brief :: String -> String
brief = take 50 . unwords . lines
