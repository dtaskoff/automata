{-# Language FlexibleInstances #-}
import FSM
import FSA
import FST
import Combinators
import Relation
import Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.List (nub)
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do
  let wordsOver = foldr (\x acc -> acc ++ map (BS.cons x) acc) [BS.empty] . S.toList

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
        in  fsa `accepts` BS.append w w'
  describe "FSA.star" $ do
    it "returns the Kleene closure of an FSA" $ do
      property' $ \w n -> star (word w) `accepts` BS.concat (replicate n w)
  describe "FSA.expand" $ do
    it "turns an FSA into a one-letter automaton" $ do
      property' $ \w u ->
        let fsa = word w
        in  fsa `accepts` u == expand fsa `accepts` u
  describe "FSA.determinise" $ do
    it "turns an NFA into a DFA" $ do
      property $ \alphabet w ->
        let nfa = allOver alphabet
            dfa = determinise nfa
        in  S.null alphabet || nfa `accepts` w == dfa `accepts` w
  describe "FSA.complement" $ do
    it "returns the complement of an FSA" $ do
      property' $ \alphabet w ->
        let fsa = FSA.total alphabet $ determinise $ word w
            cfsa = complement fsa
            ws = take 100 $ wordsOver alphabet
            accept w = fsa `accepts` w /= cfsa `accepts` w
        in  S.null alphabet || BS.null w || all accept ws
  describe "FSA.compose" $ do
    it "returns the composition (intersection) of two FSAs" $ do
      property $ \alphabet alphabet' ->
        let fsa = allOver alphabet
            fsa' = allOver alphabet'
            fsa'' = compose fsa fsa'
            ws = take 100 $ wordsOver alphabet
            ws' = take 100 $ wordsOver alphabet'
        in  S.null alphabet || S.null alphabet' ||
              all (\w -> (fsa `accepts` w && fsa' `accepts` w) == fsa'' `accepts` w) (ws ++ ws')

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
        in  fst `transduce` BS.append u u' == [BS.append v v']
  describe "FST.star" $ do
    it "returns the Kleene closure of an FSTs" $ do
      property' $ \w@(u, v) n ->
        let fst = star $ word w
            u' = BS.concat $ replicate n u
            v' = BS.concat $ replicate n v
            vs = fst `transduce` u'
            v'' = if BS.null u' then vs !! n else head vs
        in  n < 0 || v' == v''
  describe "FST.expand" $ do
    it "turns an FST into a one-letter transducer" $ do
      property' $ \w u ->
        let fst = word w
        in  fst `transduce` u == expand fst `transduce` u
  describe "FST.compose" $ do
    it "returns the composition of two FSTs" $ do
      property $ \u' v' w' ->
        let u = BS.take 10 u'
            v = BS.take 10 v'
            w = BS.take 10 w'
            fst = star $ word (u, v)
            fst' = star $ word (v, w)
            fst'' = compose fst fst'
            alphabet = S.fromList $ BS.unpack u
            ws = take 100 $ wordsOver $ alphabet
            f = transduce fst
            f' = transduce fst'
            f'' = transduce fst''
        in  S.null alphabet || BS.null u || BS.null v || BS.null w ||
              all (\w -> let l = f'' w
                             r = concatMap f' $ f w
                         in  null l && null r || head l == head r) ws

  -- | Description of Combinators
  describe "Combinators.allOver" $ do
    it "returns an automaton accepting all words over an alphabet" $ do
      property' $ \alphabet ->
        let fsa = allOver alphabet
            ws = take 100 $ foldr (\x acc -> acc ++ map (BS.cons x) acc) [BS.empty] $ S.toList alphabet
        in  S.null alphabet ||  all (fsa `accepts`) ws
  describe "Combinators.optionalReplace" $ do
    it "optional replacement transducer" $ do
      property' $ \alphabet w@(u, v) ->
        let fst = optionalReplace alphabet $ word w
            u' = if u == v then [u] else [u, v]
            u'' = fst `transduce` u
        in  S.null alphabet || BS.any (not . (`S.member` alphabet)) u || all (`elem` u'') u'
  describe "Combinators.replace" $ do
    let simpleReplace s u v =
          let n = BS.length u
              simpleReplace' s | BS.null s = BS.empty
              simpleReplace' s | u == BS.take n s = BS.append v $ simpleReplace' (BS.drop n s)
              simpleReplace' s = BS.cons (BS.head s) $ simpleReplace' $ BS.tail s
          in  simpleReplace' s
    it "replacement transducer" $ do
      property' $ \alphabet w@(u, v) ->
        let fst = replace alphabet $ word w
            sr w = simpleReplace w u v
            t = transduce fst
            ws = take 1000 $ foldr (\x acc -> acc ++ map (BS.cons x) acc) [BS.empty] $ S.toList alphabet
        in  S.null alphabet || BS.null u || BS.any (not . (`S.member` alphabet)) u || all (\w -> sr w `elem` t w) ws

property' :: Testable prop => prop -> Property
property' = withMaxSuccess 1000 . property

instance Arbitrary BS.ByteString where
  arbitrary = fmap BS.pack arbitrary

instance Arbitrary Alphabet where
  arbitrary = fmap S.fromList arbitrary
