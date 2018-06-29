module Recurse where

import Types

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


recurse :: (Eq q, Hashable q, Eq b, Hashable b) =>
  [q] -> (q -> Map a q') -> ([q'] -> Set q) -> (Map q State -> Map a q' -> Map b (Set State)) ->
  (Set q, Map q State, TransitionTable b, Int)
recurse initial aqs toSet rs =
  let go [] ps pslabels delta _ n = (ps, pslabels, delta, n)
      go (q:qs) ps pslabels delta qi n =
        let ars = aqs q
            nrs = toSet (M.elems ars) `S.difference` ps
            nrss = S.toList nrs
            pslabels' = M.fromList (zip nrss [n..]) `M.union` pslabels
            delta' = if M.null ars
                     then M.empty
                     else M.singleton qi $ rs pslabels' ars
            delta'' = unions' [delta, delta']
        in  go (qs ++ nrss) (nrs `S.union` ps) pslabels' delta'' (qi+1) (n + S.size nrs)

  in  go initial
         (S.fromList initial)
         (M.fromList $ zip initial [0..])
         M.empty 0 (length initial)
