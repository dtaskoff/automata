module Types where

import Relation

import Data.Hashable (Hashable)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Word (Word8)


type Map k v = M.HashMap k v
type Set a = S.HashSet a

type Alphabet = Set Word8
type Input = BS.ByteString
type Output = BS.ByteString

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

class (Eq a, Hashable a) => Combinable a where
  combine :: (Eq b, Hashable b) => Map a (Set b) -> Map a (Set b) -> Map a (Set (b, b))

instance Combinable Input where
  combine = M.intersectionWith setCartesian

instance Combinable (Input, Output) where
  combine aqs aqs' = M.foldlWithKey' go M.empty aqs
    where go m (a, b) qs = foldr (f a qs) m $ filter ((== b) . fst) keys
          f a qs k@(_, c) acc = M.insertWith S.union (a, c) (setCartesian qs (aqs' M.! k)) acc
          keys = M.keys aqs'
