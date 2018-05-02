import Test.Hspec
import Test.QuickCheck
import FSA


main :: IO ()
main = hspec $ do
  describe "FSA.accepts" $ do
    it "returns True if the given automaton accepts the given word" $ do
      property $ \w -> word w `accepts` w
    it "returns False if the given automaton doesn't accept the given word" $ do
      property $ \w w' -> w == w' || not (word w `accepts` w')
