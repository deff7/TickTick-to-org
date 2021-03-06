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
                 , unStatus = Normal
                 }

main :: IO ()
main = hspec $ do
  describe "Convert.convertRepeat" $ do
    let cases = [("FREQ=MONTHLY;INTERVAL=2", " +2m"),
                 ("INTERVAL=1;FREQ=DAILY", " +1d"),
                 ("FREQ=WEEKLY;WKST=MO;INTERVAL=1;BYDAY=SU", " +1w")]
    let testCase (i, o) = it (i <> " --> " <> o) $ convertRepeat i `shouldBe` o
    mapM_ testCase cases

  describe "Convert.insertItem" $ do
    let t = "root"
    let item = emptyItem { unTaskID = "123" }
    let item' = emptyItem { unTaskID = "234" }

    context "with empty path" $ do
      it "inserts item to leaf empty hash map" $ do
        insertItem (Node 1 t M.empty) item [] `shouldBe`
          (Node 1 t $ M.singleton "123" (Leaf 2 item))
      it "inserts item to leaf hash map" $ do
        insertItem (Node 1 t $ M.singleton "234" (Leaf 2 item')) item [] `shouldBe`
          (Node 1 t $ M.fromList [("123", Leaf 2 item), ("234", Leaf 2 item')])

    context "with multiple parts in path" $ do
      context "with empty node" $ do
        it "constructs new nodes along the way" $ do
          insertItem (Node 1 t M.empty) item ["foo", "bar"] `shouldBe`
            (Node 1 t $ M.singleton "foo" (Node 2 "foo" $ M.singleton "bar" (Node 3 "bar" $ M.singleton "123" (Leaf 4 item))))
      context "with non-empty node" $ do
        let want = Node 1 t $ M.singleton "foo" (Node 2 "foo" $ M.singleton "bar" (Node 3 "bar" $ M.fromList [("123", Leaf 4 item), ("234", Leaf 4 item')]))
        let root' = want

        it "modifies hashmaps along the way" $ do
          let root = Node 1 t $ M.singleton "foo" (Node 2 "foo" $ M.singleton "bar" (Node 3 "bar" $ M.singleton "123" (Leaf 4 item)))

          let got = insertItem root item' ["foo", "bar"]

          got `shouldBe` want

        it "modifies hashmaps along the way #2" $ do
          let item'' = emptyItem { unTaskID = "345" }
          let got' = insertItem root' item'' ["foo", "baz"]

          got' `shouldBe` (Node 1 t $ M.singleton "foo" (Node 2 "foo" $ M.fromList [("bar", (Node 3 "bar" $ M.fromList [("123", Leaf 4 item), ("234", Leaf 4 item')]))
                                                                               ,("baz", (Node 3 "baz" $ M.singleton "345" (Leaf 4 item'')))]))
