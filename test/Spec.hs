import qualified SpecConfig
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Config.Data" SpecConfig.spec