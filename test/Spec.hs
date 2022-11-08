import qualified ParserInternal
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "testing the auxiliary functions of the parser" ParserInternal.spec