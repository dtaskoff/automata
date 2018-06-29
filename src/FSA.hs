module FSA where

import FSM
import Recurse
import Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
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

      (ps, pslabels, d, n) =
        recurse [initial fsa'] aqs S.fromList $
          \pslabels' ars -> M.fromList
            [(a, S.singleton (pslabels' M.! qs')) | (a, qs') <- M.toList ars]

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
