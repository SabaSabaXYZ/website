import Sanitize (sanitizeTree)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Text.HTML.TagSoup (Tag(..))
import Text.HTML.TagSoup.Tree (parseTree, renderTree, transformTree, TagTree(..))
import qualified Data.Text.Lazy as T

main :: IO ()
main = hspec $ parallel $ modifyMaxSuccess (const 1000) $ do
  describe "Sanitize" $ do
    describe "sanitizeTree" $ do
      it "should set tabindex=0 for every 'pre' tag." $ property $ \(attributes, children) -> sanitizeTree (TagBranch "pre" attributes children) `shouldBe` [TagBranch "pre" (("tabindex", "0") : attributes) children]
      it "should not modify other tags." $ property $ \tag -> not (isPre tag) ==> sanitizeTree tag `shouldBe` [tag]

isPre :: TagTree T.Text -> Bool
isPre (TagBranch "pre" _ _) = True
isPre _ = False

boundedList :: (Arbitrary a) => Gen [a]
boundedList = choose (0, 10) >>= flip vectorOf arbitrary

instance Arbitrary T.Text where
  arbitrary = boundedList >>= pure . T.pack

instance Arbitrary (Tag T.Text) where
  arbitrary = chooseInt (0, 5) >>= \choice -> do
    case choice of
      0 -> arbitrary >>= \(name, attributes) -> pure $ TagOpen name attributes
      1 -> arbitrary >>= pure . TagClose
      2 -> arbitrary >>= pure . TagText
      3 -> arbitrary >>= pure . TagComment
      4 -> arbitrary >>= pure . TagWarning
      5 -> arbitrary >>= \(row, column) -> pure $ TagPosition row column

instance Arbitrary (TagTree T.Text) where
  arbitrary = chooseInt (0, 100) >>= \choice -> do
    if choice > 0 then
              arbitrary >>= pure . TagLeaf
    else
              arbitrary >>= \(tagName, attributes, tagTrees) -> pure $ TagBranch tagName attributes tagTrees
