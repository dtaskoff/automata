module FST where

import FSM

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust)


type Input = String
type Output = String
type FST = FSM (Input, Output)

transduce :: FST -> Input -> [Output]
transduce fst win = concatMap (go win "") $ initial fst
  where go win wout p =
          let ts = M.toList $ M.lookupDefault M.empty p $ delta fst
              append = if p `S.member` terminal fst && null win then ([wout] :) else id
          in  concat . append $
                [ go (fromJust mwin) (wout ++ v) q |
                  ((u, v), qs) <- ts, q <- S.toList qs
                , let mwin = stripPrefix u win, isJust mwin
                ]
