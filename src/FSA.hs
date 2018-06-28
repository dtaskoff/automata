module FSA where

import FSM
import Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust)


type FSA = FSM Input

-- | Returns True iff the automaton accepts the given word
accepts :: FSA -> Input -> Bool
accepts fsa w = not . null $ concatMap (go w) $ initial fsa
  where go "" p | p `S.member` terminal fsa = [p]
        go w' p = let ts = M.toList $ M.lookupDefault M.empty p $ delta fsa
                  in  concatMap (uncurry go)
                        [ (fromJust mv, q) |
                          (u, qs) <- ts, q <- S.toList qs
                        , let mv = BS.stripPrefix u w', isJust mv
                        ]

-- | Determinisation of an FSA
determinise :: FSA -> FSA
determinise fsa =
  let fsa' = expand $ removeEpsilonTransitions fsa
      aqs = S.foldr (M.unionWith S.union) M.empty .
              S.map (\q -> M.lookupDefault M.empty q $ delta fsa')

      go [] ps pslabels d i n = (ps, pslabels, d, n)
      go (q:qs) ps pslabels d i n =
        let aqs' = aqs q
            nqs = S.fromList (M.elems aqs') `S.difference` ps
            ps' = nqs `S.union` ps
            pslabels' = M.fromList (zip (S.toList nqs) [n..]) `M.union` pslabels
            d'' = if M.null aqs'
                  then M.empty
                  else M.singleton i $ M.fromList [(a, S.singleton (pslabels' M.! qs')) | (a, qs') <- M.toList aqs']
            d' = unions' [d, d'']
        in  go (qs ++ S.toList nqs) ps' pslabels' d' (i+1) (n + S.size nqs)

      (ps, pslabels, d, n) = go [initial fsa']
                                (S.singleton (initial fsa'))
                                (M.singleton (initial fsa') 0)
                                M.empty 0 1
  in  trim $ FSM { states = n
                 , initial = S.singleton 0
                 , terminal = S.map (pslabels M.!) $ S.filter (not . null . (terminal fsa' `S.intersection`)) ps
                 , delta = d
                 }

-- | Make an FSA total
total :: Alphabet -> FSA -> FSA
total alphabet fsa =
  let trap = states fsa
      alphabet' = S.toList alphabet
      delta' = M.fromList [ (p, aqs) | p <- [0..states fsa]
                          , let aqs = M.fromList [(BS.singleton a, S.singleton trap) | a <- alphabet']
                          ]
  in  fsa { states = states fsa + 1
          , delta = M.unionWith M.union (delta fsa) delta'
          }

-- | The complement of a total DFA
complement :: FSA -> FSA
complement fsa = fsa
  { terminal = S.fromList [0..states fsa - 1] `S.difference` terminal fsa
  }
