module FSM where

import Recurse
import Relation
import Types

import Data.Function (on)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl1')


data FSM a = FSM
  { states :: Int
  -- ^ the number of states
  -- I'll assume that FSMs have states 0..states-1
  , initial :: Set State
  , terminal :: Set State
  , delta :: TransitionTable a
  } deriving Show


-- | A machine which traverses only a single word
word :: Hash a => a -> FSM a
word a = FSM
  { states = 2
  , initial = S.singleton 0
  , terminal = S.singleton 1
  , delta = M.singleton 0 $ M.singleton a $ S.singleton 1
  }

-- | Regular opirations on FSMs
union :: Hash a => FSM a -> FSM a -> FSM a
union fsm fsm' =
  let fsm'' = rename (+ states fsm) fsm'
  in  FSM { states = states fsm + states fsm''
          , initial = initial fsm `S.union` initial fsm''
          , terminal = terminal fsm `S.union` terminal fsm''
          , delta = unions' [ delta fsm, delta fsm'' ]
          }

unions :: Hash a => [FSM a] -> FSM a
unions = foldl1' union

concatenate :: (Monoid a, Hash a) => FSM a -> FSM a -> FSM a
concatenate fsm fsm' =
  let fsm'' = rename (+ states fsm) fsm'
  in  FSM { states = states fsm + states fsm''
          , initial = initial fsm
          , terminal = terminal fsm''
          , delta = unions' [ delta fsm, delta fsm''
                            , let ts = etransitions $ initial fsm''
                              in  M.fromList [(t, ts) | t <- S.toList $ terminal fsm]
                            ]
          }

concatenates :: (Monoid a, Hash a) => [FSM a] -> FSM a
concatenates = foldl1' concatenate

star :: (Monoid a, Hash a) => FSM a -> FSM a
star fsm =
  let fsm' = plus fsm
  in  fsm' { initial = S.singleton $ states fsm'-1 }

plus :: (Monoid a, Hash a) => FSM a -> FSM a
plus fsm =
  let q = states fsm
      sq = S.singleton q
  in  fsm { states = states fsm + 1
          , terminal = sq
          , delta = unions' [ delta fsm
                            , M.singleton q $ etransitions $ initial fsm
                            , let eq = etransitions sq
                              in  M.fromList $ [(t, eq) | t <- S.toList $ terminal fsm]
                            ]
          }

-- | Rename the states in a given FSM given a mapping function
rename :: Hash a => (State -> State) -> FSM a -> FSM a
rename f fsm = fsm { initial = S.map f $ initial fsm
                   , terminal = S.map f $ terminal fsm
                   , delta = let delta' = M.map (M.map (S.map f)) $ delta fsm
                             in  M.fromList . map (\(p, m) -> (f p, m)) $ M.toList delta'
                   }
 
removeEpsilonTransitions :: (Monoid a, Hash a) => FSM a -> FSM a
removeEpsilonTransitions fsm =
  let cf = lift $ transitiveClosure $
        [(p, p) | p <- [0..states fsm-1]] ++ -- reflexiveClosure
        [ (p, q) | (p, aqs) <- M.toList $ delta fsm
        , q <- p : S.toList (M.lookupDefault S.empty mempty aqs)
        ]
      delta' = M.map (M.map (foldMap cf) . M.filterWithKey (const . (/= mempty))) $ delta fsm
  in  fsm { initial = foldMap cf $ initial fsm
          , delta = M.filter (not . M.null) delta'
          }

trim :: Hash a => FSM a -> FSM a
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
expand :: (Expandable a, Hash a) => FSM a -> FSM a
expand fsm = foldr expandTransition fsm
  [ (q, w, r) | (q, wrs) <- M.toList $ delta fsm
  , (w, rs) <- M.toList $ M.filterWithKey (const . (> 1) . size) wrs
  , r <- S.toList rs
  ]

expandTransition :: (Expandable a, Hash a) => Transition a -> FSM a -> FSM a
expandTransition (q, w, r) fsm =
  let n = size w
      delta' = M.adjust (M.delete w) q $ delta fsm
  in  fsm { states = states fsm + n-1
          , delta = unions' [ delta'
                            , M.fromList [ (t, at') |
                                           (t, (a, t')) <- zip (q : [states fsm..]) $
                                             zip (expandLabel w) $ [states fsm..states fsm + n-2] ++ [r]
                                         , let at' = M.singleton a $ S.singleton t'
                                         ]
                            ]
          }

combine :: (Expandable a, Monoid a, Monoid b, Hash a, Hash b) =>
  (Map a (Set State) -> Map a (Set State) -> Map b (Set (State, State))) -> FSM a -> FSM a -> FSM b
combine combineTransitions fsm fsm' =
  let expand' fsme = fsme' { delta = loopEpsilons (delta fsme') $ states fsme' }
        where fsme' = expand fsme

      fsme = expand' fsm
      fsme' = expand' fsm'
      aqs q = M.lookupDefault M.empty q $ delta fsme
      aqs' q' = M.lookupDefault M.empty q' $ delta fsme'
      initial' = on cartesian (S.toList . initial) fsme fsme'
      terminal' = on cartesian (S.toList . terminal) fsme fsme'

      (_, pslabels, d, n) =
        recurse initial' (\(q, q') -> combineTransitions (aqs q) (aqs' q')) S.unions $
          \pslabels' -> M.map (S.map (pslabels' M.!))

  in  trim $ removeEpsilonTransitions $
        FSM { states = n
            , initial = S.fromList $ map (pslabels M.!) initial'
            , terminal = S.fromList $ M.elems $ pslabels `M.intersection` S.toMap (S.fromList terminal')
            , delta = d
            }
