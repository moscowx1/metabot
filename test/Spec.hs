import qualified Bot.ConfigSpec
import qualified Bot.DataSpec
import qualified Bot.ResponseSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bot.DataSpec" Bot.DataSpec.spec
  describe "Bot.ConfigSpec" Bot.ConfigSpec.spec
  describe "Bot.ResponseSpec" Bot.ResponseSpec.spec