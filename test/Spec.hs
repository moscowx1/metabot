import qualified Bot.DataSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bot.DataSpec" Bot.DataSpec.spec