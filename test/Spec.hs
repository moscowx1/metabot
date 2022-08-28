import qualified Bot.DataParsingSpec
import qualified Bot.DataSpec
import qualified Bot.ResponseSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bot.DataSpec" Bot.DataSpec.spec
  describe "Bot.ConfigSpec" Bot.DataParsingSpec.spec
  describe "Bot.ResponseSpec" Bot.ResponseSpec.spec