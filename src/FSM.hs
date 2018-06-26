module FSM where

import Relation

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl1')


type Map k v = M.HashMap k v
type Set a = S.HashSet a
type State = Int
type TransitionTable a = Map State (Map a (Set State))

-- | Construct a transition table containing all elements from a list of transition tables.
unions' :: (Eq a, Hashable a) => [TransitionTable a] -> TransitionTable a
unions' = foldl1' (M.unionWith (M.unionWith S.union))
{-# INLINE unions' #-}

data FSM a = FSM
  { states :: Int
  -- ^ the number of states
  -- I'll assume that FSMs have states 0..states-1
  , initial :: Set State
  , terminal :: Set State
  , delta :: TransitionTable a
  } deriving Show


-- | A machine which traverses only a single word
word :: (Eq a, Hashable a) => a -> FSM a
word a = FSM
  { states = 2
  , initial = S.singleton 0
  , terminal = S.singleton 1
  , delta = M.singleton 0 $ M.singleton a $ S.singleton 1
  }

-- | Regular opirations on FSMs
union :: (Eq a, Hashable a) => FSM a -> FSM a -> FSM a
union fsm fsm' =
  let fsm'' = rename (+ states fsm) fsm'
  in  FSM { states = states fsm + states fsm''
          , initial = initial fsm `S.union` initial fsm''
          , terminal = terminal fsm `S.union` terminal fsm''
          , delta = unions' [ delta fsm, delta fsm'' ]
          }

concatenate :: (Eq a, Hashable a, Monoid a) => FSM a -> FSM a -> FSM a
concatenate fsm fsm' =
  let fsm'' = rename (+ states fsm) fsm'
  in  FSM { states = states fsm + states fsm''
          , initial = initial fsm
          , terminal = terminal fsm''
          , delta = unions' [ delta fsm, delta fsm''
                            , let ts = M.singleton mempty $ initial fsm''
                              in  M.fromList [(t, ts) | t <- S.toList $ terminal fsm]
                            ]
          }

star :: (Eq a, Hashable a, Monoid a) => FSM a -> FSM a
star fsm =
  let q = states fsm
      sq = S.singleton q
  in  fsm { states = states fsm + 1
          , initial = sq
          , terminal = sq
          , delta = unions' [ delta fsm
                            , M.singleton q $ M.singleton mempty $ initial fsm
                            , let eq = M.singleton mempty sq
                              in  M.fromList $ [(t, eq) | t <- S.toList $ terminal fsm]
                            ]
          }

-- | Rename the states in a given FSM given a mapping function
rename :: (Eq a, Hashable a) => (State -> State) -> FSM a -> FSM a
rename f fsm = fsm { initial = S.map f $ initial fsm
                   , terminal = S.map f $ terminal fsm
                   , delta = let delta' = M.map (M.map (S.map f)) $ delta fsm
                             in  M.fromList . map (\(p, m) -> (f p, m)) $ M.toList delta'
                   }
 
removeEpsilonTransitions :: (Eq a, Hashable a, Monoid a) => FSM a -> FSM a
removeEpsilonTransitions fsm =
  let cf = lift $ transitiveClosure [ (p, q) | (p, aqs) <- M.toList $ delta fsm
                                    , q <- S.toList $ M.lookupDefault S.empty mempty aqs
                                    ]
      delta' = M.map (M.map (foldMap cf) . M.filterWithKey (const . (/= mempty))) $ delta fsm
  in  fsm { initial = foldMap cf $ initial fsm
          , delta = M.filter (not . M.null) delta'
          }

trim :: (Eq a, Hashable a, Monoid a) => FSM a -> FSM a
trim fsm =
  let r = transitiveClosure [ (p, q) | (p, aqs) <- M.toList $ delta fsm
                            , q <- concatMap S.toList aqs
                            ]
      accessible = initial fsm `S.union` foldMap (lift r) (initial fsm)
      coaccessible = terminal fsm `S.union`
        S.fromMap (M.map (const ()) (M.filter (not . null . (`S.intersection` terminal fsm)) r))
      states' = accessible `S.intersection` coaccessible
      initial' = initial fsm `S.intersection` states'
      terminal' = terminal fsm `S.intersection` states'
      names = M.fromList $ zip (S.toList states') [0..]
  in  rename (names M.!) fsm { states = S.size states'
                             , initial = initial'
                             , terminal = terminal'
                             , delta = M.map (M.map (S.filter (`S.member` states'))) .
                                 M.filterWithKey (const . (`S.member` states')) $ delta fsm
                             }
