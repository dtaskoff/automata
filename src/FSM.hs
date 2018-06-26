module FSM where

import Data.List (nub, stripPrefix)
import Data.Maybe (isJust, fromJust)


type State = Int
type Transition a = (State, a, State)

data FSM a = FSM
  { states :: Int
  -- ^ the number of states
  -- I'll assume that FSMs have states 0..states-1
  , initial :: [State]
  , terminal :: [State]
  , delta :: [Transition a]
  } deriving Show


-- | An automaton which accepts a single word
word :: a -> FSM a
word a = FSM 2 [0] [1] [(0, a, 1)]

-- | Regular opirations on FSMs
union :: FSM a -> FSM a -> FSM a
union fsm fsm' =
  let fsm'' = rename fsm' (states fsm)
  in  FSM { states = states fsm + states fsm''
          , initial = initial fsm ++ initial fsm''
          , terminal = terminal fsm ++ terminal fsm''
          , delta = delta fsm ++ delta fsm''
          }

concatenate :: Monoid a => FSM a -> FSM a -> FSM a
concatenate fsm fsm' =
  let fsm'' = rename fsm' (states fsm)
  in  FSM { states = states fsm + states fsm'
          , initial = initial fsm
          , terminal = terminal fsm''
          , delta = delta fsm ++ delta fsm'' ++
              [(t, mempty, i) | t <- terminal fsm, i <- initial fsm'']
          }

star :: Monoid a => FSM a -> FSM a
star fsm =
  let q = states fsm
  in  fsm { states = states fsm + 1
          , initial = [q]
          , terminal = [q]
          , delta = delta fsm ++
              [(q, mempty, i) | i <- initial fsm] ++
              [(t, mempty, q) | t <- terminal fsm]
          }

removeEpsilonTransitions :: (Eq a, Monoid a) => FSM a -> FSM a
removeEpsilonTransitions fsa =
  let c' = transitiveClosure $ map (\(p, _, q) -> (p, q)) $ delta fsa
      c p = map snd $ filter (p . fst) c'
  in  fsa { initial = c (`elem` initial fsa)
          , delta = [(p, w, q) |
                      (p, w, r) <- delta fsa, w /= mempty,
                      q <- c (== r)
                    ]
          }

-- | Rename the states in a given FSA (increase them with n)
rename :: FSM a -> Int -> FSM a
rename fsm n = fsm { states = states fsm + n
                   , initial = map (+ n) $ initial fsm
                   , terminal = map (+ n) $ terminal fsm
                   , delta = map (\(p, w, q) -> (p+n, w, q+n)) $ delta fsm
                   }

-- | Transitive closure of a relation
-- (rather uneffective)
transitiveClosure :: Eq a => [(a, a)] -> [(a, a)]
transitiveClosure r =
  let r' = [(x, y) |
             (x, z) <- r,
             y <- map snd $ filter ((== z) . fst) r
           ]
  in  if length r' == length r
      then r
      else transitiveClosure r'
