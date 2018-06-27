import FSM
import FSA
import FST
import Combinators

import Test.Hspec
import Test.QuickCheck
import Data.List (nub)


main :: IO ()
main = hspec $ do
  -- | Description of FSAs
  describe "FSA.accepts" $ do
    it "returns True if the given automaton accepts the given word" $ do
      property' $ \w -> word w `accepts` w
  describe "FSA.union" $ do
    it "returns the union of two FSAs" $ do
      property' $ \w w' ->
        let fsa = word w `union` word w'
        in  fsa `accepts` w && fsa `accepts` w'
  describe "FSA.concatenate" $ do
    it "returns the concatenation of two FSAs" $ do
      property' $ \w w' ->
        let fsa = word w `concatenate` word w'
        in  fsa `accepts` (w ++ w')
  describe "FSA.star" $ do
    it "returns the Kleene closure of an FSA" $ do
      property' $ \w n -> star (word w) `accepts` concat (replicate n w)
  describe "FSA.expand" $ do
    it "turns an FSA into a one-letter automaton" $ do
      property' $ \w u ->
        let fsa = word w
        in  fsa `accepts` u == expand fsa `accepts` u
  describe "FSA.determinise" $ do
    it "turns an NFA into a DFA" $ do
      property $ \alphabet w ->
        let alphabet' = nub alphabet
            nfa = allOver alphabet'
            dfa = determinise nfa
        in  null alphabet' || nfa `accepts` w == dfa `accepts` w
  describe "FSA.complement" $ do
    it "returns the complement of an FSA" $ do
      property' $ \alphabet w ->
        let alphabet' = nub alphabet
            fsa = FSA.total alphabet $ determinise $ word w
            cfsa = complement fsa
            ws = take 100 $ foldr (\x acc -> acc ++ map (x:) acc) [""] alphabet'
            accept w = fsa `accepts` w /= cfsa `accepts` w
        in  null alphabet' || null w || all accept ws

  -- | Description of FSTs
  describe "FST.transduce" $ do
    it "transduces an input with the given transducer" $ do
      property' $ \w@(u, v) -> word w `transduce` u == [v]
  describe "FST.union" $ do
    it "returns the union of two FSTs" $ do
      property' $ \w@(u, v) w'@(u', v') ->
        let fst = word w `union` word w'
            vs = fst `transduce` u
            vs' = fst `transduce` u'
        in  if u == u'
            then vs == [v, v'] && vs' == [v, v']
            else vs == [v] && vs' == [v']
  describe "FST.concatenate" $ do
    it "returns the concatenation of two FSTs" $ do
      property' $ \w@(u, v) w'@(u', v') ->
        let fst = word w `concatenate` word w'
        in  fst `transduce` (u ++ u') == [v ++ v']
  describe "FST.star" $ do
    it "returns the Kleene closure of an FSTs" $ do
      property' $ \w@(u, v) n ->
        let fst = star $ word w
            u' = concat $ replicate n u
            v' = concat $ replicate n v
            vs = fst `transduce` u'
            v'' = if null u' then vs !! n else head vs
        in  n < 0 || v' == v''
  -- | Description of Combinators
  describe "Combinators.allOver" $ do
    it "returns an automaton accepting all words over an alphabet" $ do
      property' $ \alphabet w ->
        let alphabet' = nub alphabet
            fsa = allOver alphabet
        in  null alphabet' || or (map (`notElem` alphabet') w) || fsa `accepts` w
  describe "Combinators.optionalReplace" $ do
    it "optional replacement transducer" $ do
      property' $ \alphabet w@(u, v) ->
        let alphabet' = nub alphabet
            fst = optionalReplace alphabet' $ word w
            u' = if u == v then [u] else [u, v]
            u'' = fst `transduce` u
        in  null alphabet' || any (`notElem` alphabet) u || all (`elem` u'') u'

property' :: Testable prop => prop -> Property
property' = withMaxSuccess 1000 . property
