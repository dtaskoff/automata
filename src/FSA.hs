module FSA where

import FSM

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust)


type Input = String
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
