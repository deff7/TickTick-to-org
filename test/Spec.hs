import Test.Hspec

import Data.Time
import qualified Data.HashMap.Strict as M

import Item
import Convert

zeroTime :: UTCTime
zeroTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

emptyItem :: Item
emptyItem = Item { unFolderName = Nothing
                 , unListName = ""
                 , unTitle = ""
                 , unTags = []
                 , unIsCheckList = False
                 , unContent = ""
                 , unDueDate = Nothing
                 , unCreatedTime = zeroTime
                 , unCompletedTime = Nothing
                 , unRepeat = ""
                 , unColumnName = Nothing
                 , unTaskID = ""
                 , unChildTasks = []
                 }

main :: IO ()
main = hspec $ do
  describe "Convert.insertItem" $ do
    let item = emptyItem { unTaskID = "123" }
    let item' = emptyItem { unTaskID = "234" }

    context "with empty path" $ do
      it "inserts item to leaf empty hash map" $ do
        insertItem (Node M.empty) item [] `shouldBe`
          (Node $ M.singleton "123" (Leaf item))
      it "inserts item to leaf hash map" $ do
        insertItem (Node $ M.singleton "234" (Leaf item')) item [] `shouldBe`
          (Node $ M.fromList [("123", Leaf item), ("234", Leaf item')])

    context "with multiple parts in path" $ do
      context "with empty node" $ do
        it "constructs new nodes along the way" $ do
          insertItem (Node M.empty) item ["foo", "bar"] `shouldBe`
            (Node $ M.singleton "foo" (Node $ M.singleton "bar" (Node $ M.singleton "123" (Leaf item))))
      context "with non-empty node" $ do
        let want = Node $ M.singleton "foo" (Node $ M.singleton "bar" (Node $ M.fromList [("123", Leaf item), ("234", Leaf item')]))
        let root' = want

        it "modifies hashmaps along the way" $ do
          let root = Node $ M.singleton "foo" (Node $ M.singleton "bar" (Node $ M.singleton "123" (Leaf item)))

          let got = insertItem root item' ["foo", "bar"]

          got `shouldBe` want

        it "modifies hashmaps along the way #2" $ do
          let item'' = emptyItem { unTaskID = "345" }
          let got' = insertItem root' item'' ["foo", "baz"]

          got' `shouldBe` (Node $ M.singleton "foo" (Node $ M.fromList [("bar", (Node $ M.fromList [("123", Leaf item), ("234", Leaf item')]))
                                                                       ,("baz", (Node $ M.singleton "345" (Leaf item'')))]))
