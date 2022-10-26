import qualified DataParsingSpec
import qualified DataSpec
import qualified ResponseSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bot.DataSpec" DataSpec.spec
  describe "Bot.ConfigSpec" DataParsingSpec.spec
  describe "Bot.ResponseSpec" ResponseSpec.spec