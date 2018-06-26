import FSM
import FSA

import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  -- | Description of FSAs
  describe "FSA.accepts" $ do
    it "returns True if the given automaton accepts the given word" $ do
      property' $ \w -> word w `accepts` w
  describe "FSA.union" $ do
    it "returns the union of two FSA" $ do
      property' $ \w w' ->
        let fsa = word w `union` word w'
        in  fsa `accepts` w && fsa `accepts` w'
  describe "FSA.concatenate" $ do
    it "returns the concatenation of two FSA" $ do
      property' $ \w w' ->
        let fsa = word w `concatenate` word w'
        in  fsa `accepts` (w ++ w')
  describe "FSA.star" $ do
    it "returns the Kleene closure of an FSA" $ do
      property' $ \w n -> star (word w) `accepts` concat (replicate n w)
  describe "FSA.removeEpsilonTransitions" $ do
    it "returns an equivalent FSA only with epsilon transitions removed" $ do
      property' $ \ws ->
        let fsaU = foldr (union . word) (word "") ws
            fsaC = foldr (concatenate . word) (word "") ws
        in  and $ accepts fsaC (concat ws) : map (accepts fsaU) ("" : ws)

property' :: Testable prop => prop -> Property
property' = withMaxSuccess 1000 . property
