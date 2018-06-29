{-# Language ConstraintKinds #-}
{-# Language MultiParamTypeClasses #-}
module Types where

import Relation

import Data.Hashable (Hashable)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List (foldl1')
import Data.Word (Word8)


type Map k v = M.HashMap k v
type Set a = S.HashSet a
type Hash a = (Eq a, Hashable a)

type Alphabet = Set Word8
type Input = BS.ByteString
type Output = BS.ByteString
type State = Int
type Transition a = (State, a, State)
type TransitionTable a = Map State (Map a (Set State))

-- | Construct a transition table containing all elements from a list of transition tables.
unions' :: Hash a => [TransitionTable a] -> TransitionTable a
unions' = foldl1' (M.unionWith (M.unionWith S.union))
{-# INLINE unions' #-}

class Expandable a where
  shouldExpand :: a -> Bool
  size :: a -> Int
  expandLabel :: a -> [a]

instance Expandable Input where
  shouldExpand = (> 1) . BS.length
  size = fromIntegral . BS.length
  expandLabel = map BS.singleton . BS.unpack

instance Expandable (Input, Output) where
  shouldExpand (a, b) = shouldExpand a || shouldExpand b
  size (a, b) = max (size a) $ size b
  expandLabel (a, b) = take (size (a, b)) $
    zip (expandLabel a ++ repeat "") $ expandLabel b ++ repeat ""

class (Hash a, Hash b) => Combinable a b where
  combine :: Hash c => Map a (Set c) -> Map a (Set c) -> Map b (Set (c, c))

-- | Intersection of FSAs
-- TODO: should disable looping epsilons before applying the construction
instance Combinable Input Input where
  combine = M.intersectionWith setCartesian

-- | Composition of FSTs
instance Combinable (Input, Output) (Input, Output) where
  combine = combineMaps (\(_, b) -> filter ((== b) . fst)) (\(a, _) (_, c) -> (a, c))

-- | Product of FSAs
instance Combinable Input (Input, Output) where
  combine = combineMaps (const id) (,)

-- | Combine two maps given a way to match keys from them and merge their corresponding values
combineMaps :: (Hash a, Hash b, Hash c) =>
  (a -> [a] -> [a]) -> (a -> a -> b) -> Map a (Set c) -> Map a (Set c) -> Map b (Set (c, c))
combineMaps match merge aqs bqs = M.foldlWithKey' go M.empty aqs
  where go m a qs = foldr (f a qs) m $ match a keys
        f a qs b = M.insertWith S.union (merge a b) $ setCartesian qs (bqs M.! b)
        keys = M.keys bqs

loopEpsilons :: (Monoid a, Hash a) => TransitionTable a -> Int -> TransitionTable a
loopEpsilons delta states = unions' [ delta, M.fromList [(p, etransitions $ S.singleton p) | p <- [0..states-1]] ]

etransitions :: (Monoid a, Hash a) => Set State -> Map a (Set State)
etransitions = M.singleton mempty
