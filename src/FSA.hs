module FSA where

import FSM

import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust)


type Input = String
type FSA = FSM Input

-- | Returns True iff the automaton accepts the given word
accepts :: FSA -> Input -> Bool
accepts fsa w = any (`elem` terminal fsa) $ concatMap (go w) $ initial fsa
  where go "" p | p `elem` terminal fsa = [p]
        go w' p = concatMap (uncurry go) $
          [(fromJust mv, q) |
            (p', u, q) <- delta fsa, p' == p,
            let mv = stripPrefix u w', isJust mv
          ]
