module FSA where

import FSM

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust)


type Input = String
type Transition = (State, Input, State)
type FSA = FSM Input

-- | Returns True iff the automaton accepts the given word
accepts :: FSA -> Input -> Bool
accepts fsa w = not . null $ concatMap (go w) $ initial fsa
  where go "" p | p `S.member` terminal fsa = [p]
        go w' p = let ts = M.toList $ M.lookupDefault M.empty p $ delta fsa
                  in  concatMap (uncurry go)
                        [ (fromJust mv, q) |
                          (u, qs) <- ts, q <- S.toList qs
                        , let mv = stripPrefix u w', isJust mv
                        ]

-- | Transform to a one-letter automaton
expand :: FSA -> FSA
expand fsa = foldr expandTransition fsa
  [ (q, w, r) | (q, wrs) <- M.toList $ delta fsa
  , (w, rs) <- M.toList $ M.filterWithKey (\k _ -> length k > 1) wrs
  , r <- S.toList rs
  ]

expandTransition :: Transition -> FSA -> FSA
expandTransition (q, w, r) fsa =
  let n = length w
      delta' = M.adjust (M.delete w) q $ delta fsa
  in  fsa { states = states fsa + n - 1
          , delta = unions' [ delta'
                            , M.fromList [ (t, at') | (t, (a, t')) <-
                                              zip (q : [states fsa..]) $
                                                zip w $ [states fsa..states fsa + n - 2] ++ [r]
                                         , let at' = M.singleton [a] $ S.singleton t'
                                         ]
                            ]
          }
