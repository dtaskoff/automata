module FSM where

import Relation
import Types

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl1')


type Map k v = M.HashMap k v
type Set a = S.HashSet a
type State = Int
type Transition a = (State, a, State)
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

unions :: (Eq a, Hashable a) => [FSM a] -> FSM a
unions = foldl1' union

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

concatenates :: (Eq a, Hashable a, Monoid a) => [FSM a] -> FSM a
concatenates = foldl1' concatenate

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
  let cf = lift $ transitiveClosure $
        [(p, p) | p <- [0..states fsm - 1]] ++
        [ (p, q) | (p, aqs) <- M.toList $ delta fsm
        , q <- p : S.toList (M.lookupDefault S.empty mempty aqs)
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

-- | Transform to a one-letter machine
expand :: (Eq a, Hashable a, Expandable a) => FSM a -> FSM a
expand fsm = foldr expandTransition fsm
  [ (q, w, r) | (q, wrs) <- M.toList $ delta fsm
  , (w, rs) <- M.toList $ M.filterWithKey (const . shouldExpand) wrs
  , r <- S.toList rs
  ]

expandTransition :: (Eq a, Hashable a, Expandable a) => Transition a -> FSM a -> FSM a
expandTransition (q, w, r) fsm =
  let n = size w
      delta' = M.adjust (M.delete w) q $ delta fsm
  in  fsm { states = states fsm + n - 1
          , delta = unions' [ delta'
                            , M.fromList [ (t, at') |
                                           (t, (a, t')) <- zip (q : [states fsm..]) $
                                             zip (expandLabel w) $ [states fsm..states fsm + n - 2] ++ [r]
                                         , let at' = M.singleton a $ S.singleton t'
                                         ]
                            ]
          }

product :: (Eq a, Hashable a) =>
  [(State, State)] -> TransitionTable a -> TransitionTable a -> (Map (State, State) State, TransitionTable a)
product [] _ _ = (M.empty, M.empty)
product initial delta delta' =
  let aqs = M.lookupDefault M.empty
      n = length initial

      go [] _ pslabels d _ _ = (pslabels, d)
      go ((q, q'):qs) ps pslabels d i n =
        let ars = aqs q delta
            ars' = aqs q' delta'
            ars'' = M.intersectionWith (\rs rs' -> S.fromList $ cartesian (S.toList rs) (S.toList rs')) ars ars'
            nrs = S.unions (M.elems ars'') `S.difference` ps
            pslabels' = M.fromList (zip (S.toList nrs) [n..]) `M.union` pslabels
            d' = unions' [ d
                         , if M.null ars''
                           then M.empty
                           else M.singleton i $ M.map (S.map (pslabels' M.!)) ars''
                         ]
        in go (qs ++ S.toList nrs) (nrs `S.union` ps) pslabels' d' (i+1) (n + S.size nrs)

  in  go initial
         (S.fromList initial)
         (M.fromList $ zip initial [0..])
         M.empty 0 n

intersect :: (Eq a, Hashable a) => FSM a -> FSM a -> FSM a
intersect fsm fsm' =
  let (labels, delta'') = FSM.product initial' (delta fsm) (delta fsm')
      cartesian' s s' = cartesian (S.toList s) (S.toList s')
      initial' = cartesian' (initial fsm) (initial fsm')
      terminal' = cartesian' (terminal fsm) (terminal fsm')
  in  FSM { states = M.size labels
          , initial = S.fromList $ map (labels M.!) initial'
          , terminal = S.fromList $ M.elems $ labels `M.intersection` S.toMap (S.fromList terminal')
          , delta = delta''
          }
