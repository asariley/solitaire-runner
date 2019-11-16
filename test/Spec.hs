import ClassyPrelude
import Test.Hspec (hspec)
import qualified KlondikeSpec
import qualified SinglePlayerSpec

main :: IO ()
main = hspec $ do
  KlondikeSpec.spec
  SinglePlayerSpec.spec
