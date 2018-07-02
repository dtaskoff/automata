module Combinators where

import FSM
import FSA
import FST
import Relation
import Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


intersect :: FSA -> FSA -> FSA
intersect = combine $ M.intersectionWith setCartesian

compose :: FST -> FST -> FST
compose = combine $ combineMaps (\(_, b) -> filter ((== b) . fst)) (\(a, _) (_, c) -> (a, c))

product :: FSA -> FSA -> FST
product = combine $ combineMaps (const id) (,)

-- | Combine two maps given a way to match keys from them and merge their corresponding values
combineMaps :: (Hash a, Hash b, Hash c) =>
  (a -> [a] -> [a]) -> (a -> a -> b) -> Map a (Set c) -> Map a (Set c) -> Map b (Set (c, c))
combineMaps match merge aqs bqs = M.foldlWithKey' go M.empty aqs
  where go m a qs = foldr (f a qs) m $ match a keys
        f a qs b = M.insertWith S.union (merge a b) $ setCartesian qs (bqs M.! b)
        keys = M.keys bqs

transform :: (Hash a, Hash b) => (a -> b) -> FSM a -> FSM b
transform f fsm = fsm
  { delta = M.map (M.fromList . map (\(a, b) -> (f a, b)) . M.toList) $ delta fsm
  }

domain :: FST -> FSA
domain = transform fst

range :: FST -> FSA
range = transform snd

inverse :: FST -> FST
inverse = transform (\(u, v) -> (v, u))

identity :: FSA -> FST
identity = transform (\u -> (u, u))

allOver :: Alphabet -> FSA
allOver = star . unions . map (word . BS.singleton) . S.toList

optionalReplace :: Alphabet -> FST -> FST
optionalReplace = replace' . identity . allOver

replace :: Alphabet -> FST -> FST
replace alphabet fst = replace' ide fst
  where ide = union (word mempty) $ identity $ complement' $
          concatenates [all', domain fst, all']
        complement' = complement . total alphabet . determinise
        all' = allOver alphabet

replace' :: FST -> FST -> FST
replace' ide fst = concatenate ide $ star $ concatenate fst ide

conflictFree :: FST -> Bool
conflictFree t =
  let domaint = determinise $ domain t
      idomaint = identity domaint
      s = project fst t
      sstar = allOver s
      splus = determinise $ plus $ unions $ map (word . BS.singleton) $ S.toList s
      isplus = identity splus
      spluse = Combinators.product splus $ word BS.empty

      prefixes = determinise $ range $ compose idomaint $ concatenate isplus spluse
      suffixes = determinise $ range $ compose idomaint $ concatenate spluse isplus
      overlaps = intersect prefixes suffixes

      left = concatenates [splus, domaint, sstar]
      right = concatenates [sstar, domaint, splus]
      containments = intersect (union left right) domaint
  in  S.null (project id overlaps) && S.null (project id containments)

project :: (a -> BS.ByteString) -> FSM a -> Alphabet
project p = S.fromList . concatMap (concatMap (BS.unpack . p) . M.keys) . M.elems . delta
